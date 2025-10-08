use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::{io, net::TcpListener};
use tower_lsp::{
    async_trait,
    jsonrpc::Error,
    lsp_types::{
        notification::Notification, Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams,
        DidCloseTextDocumentParams, DidOpenTextDocumentParams, ExecuteCommandOptions,
        ExecuteCommandParams, Hover, HoverContents, HoverParams, HoverProviderCapability,
        InitializeParams, InitializeResult, MarkupContent, MarkupKind, MessageType, Position,
        Range, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
    },
    Client, LanguageServer, LspService, Server,
};

#[derive(Debug, Serialize, Deserialize)]
struct NotificationParams {
    title: String,
    message: String,
    description: String,
}

enum CustomNotification {}

impl Notification for CustomNotification {
    type Params = NotificationParams;
    const METHOD: &'static str = "custom/notification";
}

#[derive(Debug, Clone)]
struct ManifoldAttribute {
    name: String,
    range: Range,
    start_offset: usize,
    end_offset: usize,
}

#[derive(Debug, Clone)]
struct ManifoldDocument {
    attributes: Vec<ManifoldAttribute>,
    line_index: LineIndex,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn attribute_names(source: &str) -> Vec<String> {
        ManifoldDocument::parse(source.to_owned())
            .attributes
            .into_iter()
            .map(|attr| attr.name)
            .collect()
    }

    #[test]
    fn skips_elements_without_register() {
        let html = r#"
            <div>
                <button :onclick="count++">Click</button>
                <input data-mf-sync="value" />
            </div>
        "#;

        assert!(attribute_names(html).is_empty());
    }

    #[test]
    fn captures_registered_prefixed_attributes_only() {
        let html = r#"
            <section data-mf-register>
                <button :onclick="count++">Click</button>
                <input data-mf-sync="value" />
                <div data-mf-register="nested" class="plain" :if="show"></div>
                <span class="plain" data-something="else"></span>
            </section>
        "#;

        let names = attribute_names(html);
        assert_eq!(names, vec![":if"]);
    }
}

#[derive(Debug)]
struct ParsedAttribute {
    name: String,
    name_lower: String,
    span_start: usize,
    span_end: usize,
    value_range: Option<(usize, usize)>,
}

#[derive(Debug, Clone)]
struct LineIndex {
    line_starts: Vec<usize>,
    text_len: usize,
}

impl LineIndex {
    fn new(text: &str) -> Self {
        let mut line_starts = vec![0];
        for (idx, ch) in text.char_indices() {
            if ch == '\n' {
                let next = idx + ch.len_utf8();
                line_starts.push(next);
            }
        }
        Self {
            line_starts,
            text_len: text.len(),
        }
    }

    fn position_at(&self, offset: usize) -> Position {
        let line = match self.line_starts.binary_search(&offset) {
            Ok(index) => index,
            Err(index) => index.saturating_sub(1),
        };
        let line_start = self.line_starts.get(line).copied().unwrap_or(0);
        Position::new(line as u32, (offset.saturating_sub(line_start)) as u32)
    }

    fn offset_at(&self, position: &Position) -> Option<usize> {
        let line = position.line as usize;
        let character = position.character as usize;
        let line_start = *self.line_starts.get(line)?;
        let line_end = self
            .line_starts
            .get(line + 1)
            .copied()
            .unwrap_or(self.text_len);
        let offset = line_start + character;
        if offset > line_end {
            None
        } else {
            Some(offset)
        }
    }
}

impl ManifoldDocument {
    fn parse(text: String) -> Self {
        let line_index = LineIndex::new(&text);
        let attributes = Self::extract_manifold_attributes(&text, &line_index);
        Self {
            attributes,
            line_index,
        }
    }

    fn extract_manifold_attributes(text: &str, line_index: &LineIndex) -> Vec<ManifoldAttribute> {
        let bytes = text.as_bytes();
        let len = bytes.len();
        let mut idx = 0;
        let mut results = Vec::new();

        while idx < len {
            let Some(rel) = text[idx..].find('<') else {
                break;
            };
            let tag_start = idx + rel;
            idx = tag_start + 1;

            if idx >= len {
                break;
            }

            let next_char = bytes[idx];
            if matches!(next_char, b'/' | b'!' | b'?') {
                continue;
            }

            let mut cursor = idx;
            let mut in_quote: Option<u8> = None;
            while cursor < len {
                let ch = bytes[cursor];
                if let Some(quote) = in_quote {
                    if ch == quote {
                        in_quote = None;
                    }
                } else if ch == b'"' || ch == b'\'' {
                    in_quote = Some(ch);
                } else if ch == b'>' {
                    break;
                }
                cursor += 1;
            }

            if cursor >= len {
                break;
            }

            let tag_end = cursor;
            idx = cursor + 1;

            let mut attr_cursor = tag_start + 1;
            while attr_cursor < tag_end {
                let ch = bytes[attr_cursor];
                if ch.is_ascii_whitespace() {
                    attr_cursor += 1;
                    break;
                }
                if matches!(ch, b'/' | b'>') {
                    break;
                }
                attr_cursor += 1;
            }

            let mut has_data_mf_register = false;
            let mut attributes: Vec<ParsedAttribute> = Vec::new();

            while attr_cursor < tag_end {
                while attr_cursor < tag_end && bytes[attr_cursor].is_ascii_whitespace() {
                    attr_cursor += 1;
                }
                if attr_cursor >= tag_end {
                    break;
                }
                if bytes[attr_cursor] == b'/' {
                    attr_cursor += 1;
                    continue;
                }

                let attr_name_start = attr_cursor;
                while attr_cursor < tag_end
                    && !bytes[attr_cursor].is_ascii_whitespace()
                    && !matches!(bytes[attr_cursor], b'=' | b'/')
                {
                    attr_cursor += 1;
                }
                let attr_name_end = attr_cursor;
                if attr_name_start >= attr_name_end {
                    break;
                }

                while attr_cursor < tag_end && bytes[attr_cursor].is_ascii_whitespace() {
                    attr_cursor += 1;
                }

                let (attr_end, value_range) = if attr_cursor < tag_end && bytes[attr_cursor] == b'='
                {
                    attr_cursor += 1;
                    while attr_cursor < tag_end && bytes[attr_cursor].is_ascii_whitespace() {
                        attr_cursor += 1;
                    }

                    if attr_cursor < tag_end {
                        if matches!(bytes[attr_cursor], b'"' | b'\'') {
                            let quote = bytes[attr_cursor];
                            attr_cursor += 1;
                            let value_start = attr_cursor;
                            while attr_cursor < tag_end && bytes[attr_cursor] != quote {
                                attr_cursor += 1;
                            }
                            let value_end = attr_cursor;
                            let range = Some((value_start, value_end));
                            if attr_cursor < tag_end {
                                attr_cursor += 1;
                            }
                            (attr_cursor, range)
                        } else {
                            let value_start = attr_cursor;
                            while attr_cursor < tag_end
                                && !bytes[attr_cursor].is_ascii_whitespace()
                                && bytes[attr_cursor] != b'/'
                            {
                                attr_cursor += 1;
                            }
                            let value_end = attr_cursor;
                            (attr_cursor, Some((value_start, value_end)))
                        }
                    } else {
                        (attr_cursor, None)
                    }
                } else {
                    (attr_cursor, None)
                };

                let name = text[attr_name_start..attr_name_end].to_string();
                let name_lower = name.to_ascii_lowercase();
                if name_lower == "data-mf-register" {
                    has_data_mf_register = true;
                } else {
                    attributes.push(ParsedAttribute {
                        name,
                        name_lower,
                        span_start: attr_name_start,
                        span_end: attr_end,
                        value_range,
                    });
                }
            }

            if has_data_mf_register {
                for attr in attributes.into_iter() {
                    if attr.name.starts_with(':') || attr.name_lower.starts_with("data-mf") {
                        let (start, end) = match attr.value_range {
                            Some((value_start, value_end)) if value_end > value_start => {
                                (value_start, value_end)
                            }
                            _ => (attr.span_start, attr.span_end),
                        };
                        let range =
                            Range::new(line_index.position_at(start), line_index.position_at(end));
                        results.push(ManifoldAttribute {
                            name: attr.name,
                            range,
                            start_offset: start,
                            end_offset: end,
                        });
                    }
                }
            }
        }

        results
    }

    fn manifold_attribute_at(&self, position: &Position) -> Option<&ManifoldAttribute> {
        let offset = self.line_index.offset_at(position)?;
        self.attributes
            .iter()
            .find(|attr| offset >= attr.start_offset && offset < attr.end_offset)
    }
}

struct Backend {
    client: Client,
    documents: DashMap<Url, ManifoldDocument>,
}

impl Backend {
    fn update_document(&self, uri: Url, text: String) {
        let document = ManifoldDocument::parse(text);
        self.documents.insert(uri, document);
    }

    fn remove_document(&self, uri: &Url) {
        self.documents.remove(uri);
    }

    fn manifold_attribute_at(&self, uri: &Url, position: &Position) -> Option<ManifoldAttribute> {
        self.documents
            .get(uri)
            .and_then(|doc| doc.manifold_attribute_at(position).cloned())
    }

    async fn refresh_diagnostics(&self, uri: Url) {
        let diagnostics = self
            .documents
            .get(&uri)
            .map(|doc| {
                doc.attributes
                    .iter()
                    .map(|attr| Diagnostic {
                        range: attr.range.clone(),
                        severity: Some(DiagnosticSeverity::INFORMATION),
                        source: Some("manifold".to_string()),
                        message: format!(
                            "Manifold attribute `{}` recognized on a registered element",
                            attr.name
                        ),
                        ..Diagnostic::default()
                    })
                    .collect()
            })
            .unwrap_or_default();

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult, Error> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec![String::from("custom.notification")],
                    ..Default::default()
                }),
                ..ServerCapabilities::default()
            },
            ..Default::default()
        })
    }

    async fn shutdown(&self) -> Result<(), Error> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        self.update_document(uri.clone(), params.text_document.text);
        self.refresh_diagnostics(uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().last() {
            self.update_document(uri.clone(), change.text);
            self.refresh_diagnostics(uri).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.remove_document(&uri);
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>, Error> {
        let position_params = params.text_document_position_params;
        let uri = position_params.text_document.uri;
        let position = position_params.position;

        if let Some(attribute) = self.manifold_attribute_at(&uri, &position) {
            let range = attribute.range.clone();
            let hover = Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!(
                        "`{}` is treated as a Manifold-specific attribute because its element is registered with `data-mf-register`.",
                        attribute.name
                    ),
                }),
                range: Some(range),
            };
            Ok(Some(hover))
        } else {
            Ok(None)
        }
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>, Error> {
        if params.command == "custom.notification" {
            self.client
                .send_notification::<CustomNotification>(NotificationParams {
                    title: String::from("Hello!"),
                    message: String::from("This is a custom notification."),
                    description: String::from("Sent from the Rust LSP server."),
                })
                .await;

            self.client
                .log_message(
                    MessageType::INFO,
                    format!("Command executited successfully with params: {:?}", params),
                )
                .await;

            Ok(None)
        } else {
            Err(Error::invalid_request())
        }
    }
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt().init();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: DashMap::new(),
    });

    if let Ok(addr) = std::env::var("MANIFOLD_LSP_TCP") {
        let listener = TcpListener::bind(addr).await.unwrap();
        let (stream, _) = listener.accept().await.unwrap();
        let (read, write) = tokio::io::split(stream);
        Server::new(read, write, socket).serve(service).await;
    } else {
        let stdin = io::stdin();
        let stdout = io::stdout();
        Server::new(stdin, stdout, socket).serve(service).await;
    }
}
