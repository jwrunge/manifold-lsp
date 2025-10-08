use crate::expression::{parse_expression, ExpressionTokenKind};
use dashmap::DashMap;
use serde_json::Value;
use tower_lsp::{
    async_trait,
    jsonrpc::Error,
    lsp_types::{
        Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, ExecuteCommandOptions, ExecuteCommandParams, Hover,
        HoverContents, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult,
        MarkupContent, MarkupKind, MessageType, Position, SemanticToken, SemanticTokenType,
        SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
        SemanticTokensParams, SemanticTokensResult, SemanticTokensServerCapabilities,
        ServerCapabilities, TextDocumentContentChangeEvent, TextDocumentSyncCapability,
        TextDocumentSyncKind, Url, WorkDoneProgressOptions,
    },
    Client, LanguageServer,
};

use super::attribute::{ManifoldAttribute, ManifoldAttributeKind};
use super::document::{DocumentSemanticToken, ManifoldDocument};
use super::lineindex::LineIndex;
use super::notification::{ManifoldNotification, NotificationParams};

const SEMANTIC_TOKEN_TYPES: [SemanticTokenType; 5] = [
    SemanticTokenType::KEYWORD,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::NUMBER,
    SemanticTokenType::STRING,
    SemanticTokenType::OPERATOR,
];

fn semantic_token_kind_index(kind: ExpressionTokenKind) -> u32 {
    match kind {
        ExpressionTokenKind::Keyword => 0,
        ExpressionTokenKind::Identifier => 1,
        ExpressionTokenKind::Number => 2,
        ExpressionTokenKind::String => 3,
        ExpressionTokenKind::Operator => 4,
    }
}

fn encode_semantic_tokens(tokens: &[DocumentSemanticToken]) -> Vec<SemanticToken> {
    let mut data = Vec::with_capacity(tokens.len());
    let mut previous_line = 0;
    let mut previous_start = 0;

    for token in tokens {
        let delta_line = token.line.saturating_sub(previous_line);
        let delta_start = if delta_line == 0 {
            token.start_char.saturating_sub(previous_start)
        } else {
            token.start_char
        };

        data.push(SemanticToken {
            delta_line,
            delta_start,
            length: token.length,
            token_type: semantic_token_kind_index(token.kind),
            token_modifiers_bitset: 0,
        });

        previous_line = token.line;
        previous_start = token.start_char;
    }

    data
}

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
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions::default(),
                            legend: SemanticTokensLegend {
                                token_types: SEMANTIC_TOKEN_TYPES.iter().cloned().collect(),
                                token_modifiers: Vec::new(),
                            },
                            range: None,
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
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
            let base_message = match attribute.kind {
                ManifoldAttributeKind::Attribute => format!(
                    "`{}` is treated as a Manifold-specific attribute because its element is registered with `data-mf-register`.",
                    attribute.name
                ),
                ManifoldAttributeKind::TextExpression => format!(
                    "Expression `{}` is evaluated by Manifold because its ancestor is registered with `data-mf-register`.",
                    attribute.name
                ),
            };

            let highlighted = if let Some(expr) = attribute
                .expression
                .as_ref()
                .map(|expr| expr.trim())
                .filter(|expr| !expr.is_empty())
            {
                if parse_expression(expr).is_ok() {
                    let sanitized = expr.replace("```", "`\u{200b}```");
                    format!("{base}\n\n```ts\n{sanitized}\n```", base = &base_message)
                } else {
                    base_message.clone()
                }
            } else {
                base_message.clone()
            };

            let hover = Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: highlighted,
                }),
                range: Some(range),
            };
            Ok(Some(hover))
        } else {
            Ok(None)
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>, Error> {
        let uri = params.text_document.uri;

        let data = self
            .documents
            .get(&uri)
            .map(|doc| encode_semantic_tokens(&doc.parsed.semantic_tokens()))
            .unwrap_or_default();

        let tokens = SemanticTokens {
            result_id: None,
            data,
        };

        Ok(Some(tokens.into()))
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
