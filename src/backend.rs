use dashmap::DashMap;
use serde_json::Value;
use tower_lsp::{
    async_trait,
    jsonrpc::Error,
    lsp_types::{
        Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, ExecuteCommandOptions, ExecuteCommandParams, Hover,
        HoverContents, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult,
        MarkupContent, MarkupKind, MessageType, Position, ServerCapabilities,
        TextDocumentContentChangeEvent, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
    },
    Client, LanguageServer,
};

use super::attribute::{ManifoldAttribute, ManifoldAttributeKind};
use super::document::ManifoldDocument;
use super::lineindex::LineIndex;
use super::notification::{ManifoldNotification, NotificationParams};

#[derive(Debug, Clone)]
struct StoredDocument {
    text: String,
    parsed: ManifoldDocument,
}

impl StoredDocument {
    fn new(text: String) -> Self {
        let parsed = ManifoldDocument::parse(&text);
        Self { text, parsed }
    }

    fn apply_content_changes(&mut self, changes: Vec<TextDocumentContentChangeEvent>) {
        if changes.is_empty() {
            return;
        }

        for change in changes {
            if let Some(range) = change.range {
                let line_index = LineIndex::new(&self.text);
                let Some(start) = line_index.offset_at(&range.start) else {
                    self.text = change.text;
                    continue;
                };
                let Some(end) = line_index.offset_at(&range.end) else {
                    self.text = change.text;
                    continue;
                };

                if start <= end && end <= self.text.len() {
                    self.text.replace_range(start..end, &change.text);
                } else {
                    self.text = change.text;
                }
            } else {
                self.text = change.text;
            }
        }

        self.parsed = ManifoldDocument::parse(&self.text);
    }
}

pub struct Backend {
    client: Client,
    documents: DashMap<Url, StoredDocument>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
        }
    }

    pub fn update_document(&self, uri: Url, text: String) {
        let document = StoredDocument::new(text);
        self.documents.insert(uri, document);
    }

    pub fn remove_document(&self, uri: &Url) {
        self.documents.remove(uri);
    }

    pub fn manifold_attribute_at(
        &self,
        uri: &Url,
        position: &Position,
    ) -> Option<ManifoldAttribute> {
        self.documents
            .get(uri)
            .and_then(|doc| doc.parsed.manifold_attribute_at(position).cloned())
    }

    pub async fn refresh_diagnostics(&self, uri: Url) {
        let (diagnostics, count) = self
            .documents
            .get(&uri)
            .map(|doc| {
                let diagnostics = doc
                    .parsed
                    .attributes
                    .iter()
                    .map(|attr| Diagnostic {
                        range: attr.range.clone(),
                        severity: Some(DiagnosticSeverity::INFORMATION),
                        source: Some("manifold".to_string()),
                        message: match attr.kind {
                            ManifoldAttributeKind::Attribute => format!(
                                "Manifold attribute `{}` recognized on a registered element",
                                attr.name
                            ),
                            ManifoldAttributeKind::TextExpression => format!(
                                "Manifold expression `{}` recognized inside a registered element",
                                attr.name
                            ),
                        },
                        ..Diagnostic::default()
                    })
                    .collect();
                (diagnostics, doc.parsed.attributes.len())
            })
            .unwrap_or_else(|| (Vec::new(), 0));

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
        let _ = self
            .client
            .log_message(
                MessageType::LOG,
                format!("Manifold LSP published {count} diagnostics for {uri}"),
            )
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
                    TextDocumentSyncKind::INCREMENTAL,
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
        let DidChangeTextDocumentParams {
            text_document,
            content_changes,
        } = params;

        let uri = text_document.uri;

        if let Some(mut document) = self.documents.get_mut(&uri) {
            document.apply_content_changes(content_changes);
            drop(document);
            self.refresh_diagnostics(uri).await;
            return;
        }

        if let Some(change) = content_changes.into_iter().last() {
            if change.range.is_none() {
                self.update_document(uri.clone(), change.text);
                self.refresh_diagnostics(uri).await;
            } else {
                let _ = self
                    .client
                    .log_message(
                        MessageType::WARNING,
                        format!(
                            "Received incremental change for unopened document {uri}. Ignoring update."
                        ),
                    )
                    .await;
            }
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
                .send_notification::<ManifoldNotification>(NotificationParams {
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
