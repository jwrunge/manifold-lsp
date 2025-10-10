use crate::expression::{parse_expression, ExpressionTokenKind};
use dashmap::DashMap;
use serde_json::Value;
use tower_lsp::{
    async_trait,
    jsonrpc::Error,
    lsp_types::{
        DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
        ExecuteCommandOptions, ExecuteCommandParams, GotoDefinitionParams, GotoDefinitionResponse,
        Hover, HoverContents, HoverParams, HoverProviderCapability, InitializeParams,
        InitializeResult, InlayHint, InlayHintParams, Location, MarkupContent, MarkupKind,
        MessageType, Position as LspPosition, Range as LspRange, ReferenceParams, SemanticToken,
        SemanticTokenType, SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend,
        SemanticTokensOptions, SemanticTokensParams, SemanticTokensResult,
        SemanticTokensServerCapabilities, ServerCapabilities, TextDocumentContentChangeEvent,
        TextDocumentSyncCapability, TextDocumentSyncKind, Url, WorkDoneProgressOptions,
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
    fn new_with_uri(text: String, uri: &Url) -> Self {
        // Derive base_dir from the document URI
        let base_dir = uri
            .to_file_path()
            .ok()
            .and_then(|p| p.parent().map(|pp| pp.to_path_buf()));
        let parsed = if let Some(base) = base_dir.as_deref() {
            ManifoldDocument::parse_with_base(&text, Some(base))
        } else {
            ManifoldDocument::parse(&text)
        };
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

        // Keep base_dir the same; rebuild with parse_with_base if present
        let base = self.parsed.base_dir.as_deref();
        self.parsed = if let Some(base_dir) = base {
            ManifoldDocument::parse_with_base(&self.text, Some(base_dir))
        } else {
            ManifoldDocument::parse(&self.text)
        };
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
        let document = StoredDocument::new_with_uri(text, &uri);
        self.documents.insert(uri, document);
    }

    pub fn remove_document(&self, uri: &Url) {
        self.documents.remove(uri);
    }

    pub async fn refresh_diagnostics(&self, uri: Url) {
        let diagnostics = self
            .documents
            .get(&uri)
            .map(|document| document.parsed.diagnostics())
            .unwrap_or_default();

        let count = diagnostics.len();

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;

        let _ = self
            .client
            .log_message(
                MessageType::LOG,
                format!("Manifold LSP reported {count} diagnostics for {uri}"),
            )
            .await;
    }
}

fn property_identifier_at(
    attribute: &ManifoldAttribute,
    line_index: &LineIndex,
    position: &LspPosition,
) -> Option<String> {
    let expr = attribute.expression.as_ref()?;
    let (span_start, _) = attribute.expression_span?;
    let offset = line_index.offset_at(position)?;
    if offset < span_start || offset >= attribute.end_offset {
        return None;
    }

    let rel_pos = offset.saturating_sub(span_start);
    let bytes = expr.as_bytes();
    let mut start = rel_pos.min(bytes.len());
    while start > 0 {
        let ch = bytes[start - 1] as char;
        if ch.is_alphanumeric() || ch == '_' || ch == '$' || ch == '.' {
            start -= 1;
        } else {
            break;
        }
    }

    let mut end = rel_pos.min(bytes.len());
    while end < bytes.len() {
        let ch = bytes[end] as char;
        if ch.is_alphanumeric() || ch == '_' || ch == '$' || ch == '.' {
            end += 1;
        } else {
            break;
        }
    }

    if start >= end {
        return None;
    }

    let token = &expr[start..end];
    let ident = token.split('.').next().unwrap_or("");
    if ident.is_empty() {
        return None;
    }

    Some(ident.to_string())
}

fn position_within_range(position: &LspPosition, start: &LspPosition, end: &LspPosition) -> bool {
    if position.line < start.line
        || (position.line == start.line && position.character < start.character)
    {
        return false;
    }

    if position.line > end.line
        || (position.line == end.line && position.character >= end.character)
    {
        return false;
    }

    true
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
                definition_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),
                references_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),
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
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>, Error> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        // Find attribute under cursor and token text
        let maybe_attr = doc.parsed.manifold_attribute_at(&position);
        let Some(attr) = maybe_attr else {
            return Ok(None);
        };
        // Jump to state definition when clicking state name in data-mf-register
        if attr.name.eq_ignore_ascii_case("data-mf-register") {
            if let Some(expr) = &attr.expression {
                let html_path = uri.to_file_path().ok();
                let (state_idx, _unresolved) =
                    crate::state::build_state_name_index(&doc.text, html_path.as_deref());
                let name = expr.trim().trim_matches('"').trim_matches('\'');
                if let Some(loc) = state_idx.get(name) {
                    let range = LspRange::new(
                        LspPosition::new(loc.line, loc.character),
                        LspPosition::new(loc.line, loc.character + loc.length),
                    );
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri: loc.uri.clone(),
                        range,
                    })));
                }
            }
            return Ok(None);
        }

        let li = &doc.parsed.line_index;
        let Some(ident) = property_identifier_at(attr, li, &position) else {
            return Ok(None);
        };

        // Resolve state name
        let state_name = attr
            .state_name
            .clone()
            .unwrap_or_else(|| "default".to_string());

        // Build definition index from current document
        let html_path = uri.to_file_path().ok();
        let (index, _unresolved) =
            crate::state::build_definition_index(&doc.text, html_path.as_deref());

        if let Some(loc) = index.get(&(state_name.clone(), ident.clone())) {
            let range = LspRange::new(
                LspPosition::new(loc.line, loc.character),
                LspPosition::new(loc.line, loc.character + loc.length),
            );
            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri: loc.uri.clone(),
                range,
            })));
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>, Error> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;

        let mut results: Vec<Location> = Vec::new();

        if let Some(doc) = self.documents.get(&uri) {
            if let Some(attribute) = doc.parsed.manifold_attribute_at(&position) {
                if attribute.name.eq_ignore_ascii_case("data-mf-register") {
                    return Ok(Some(results));
                }

                let li = &doc.parsed.line_index;
                if let Some(identifier) = property_identifier_at(attribute, li, &position) {
                    let state_name = attribute
                        .state_name
                        .clone()
                        .unwrap_or_else(|| "default".to_string());

                    for range in doc.parsed.property_references(&state_name, &identifier) {
                        results.push(Location {
                            uri: uri.clone(),
                            range,
                        });
                    }

                    if include_declaration {
                        let html_path = uri.to_file_path().ok();
                        let (index, _unresolved) =
                            crate::state::build_definition_index(&doc.text, html_path.as_deref());
                        if let Some(def) = index.get(&(state_name.clone(), identifier.clone())) {
                            results.push(Location {
                                uri: def.uri.clone(),
                                range: LspRange::new(
                                    LspPosition::new(def.line, def.character),
                                    LspPosition::new(def.line, def.character + def.length),
                                ),
                            });
                        }
                    }
                }
            }

            return Ok(Some(results));
        }

        for entry in self.documents.iter() {
            let html_uri = entry.key().clone();
            let doc = entry.value();
            let html_path = html_uri.to_file_path().ok();
            let (index, _unresolved) =
                crate::state::build_definition_index(&doc.text, html_path.as_deref());

            for ((state_name, prop_name), location) in index.iter() {
                if location.uri != uri {
                    continue;
                }

                let start = LspPosition::new(location.line, location.character);
                let end = LspPosition::new(location.line, location.character + location.length);
                if !position_within_range(&position, &start, &end) {
                    continue;
                }

                for range in doc.parsed.property_references(state_name, prop_name) {
                    results.push(Location {
                        uri: html_uri.clone(),
                        range,
                    });
                }

                if include_declaration {
                    results.push(Location {
                        uri: location.uri.clone(),
                        range: LspRange::new(start, end),
                    });
                }
            }
        }

        Ok(Some(results))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>, Error> {
        let uri = params.text_document.uri;
        let range = params.range;
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(Some(Vec::new()));
        };

        let hints = doc.parsed.inlay_hints(&range);
        Ok(Some(hints))
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

        if let Some(document) = self.documents.get(&uri) {
            if let Some(attribute) = document.parsed.manifold_attribute_at(&position).cloned() {
                let range = attribute.range.clone();
                let intro_message = match attribute.kind {
                ManifoldAttributeKind::Attribute => format!(
                    "`{}` is treated as a Manifold-specific attribute because its element is registered with `data-mf-register`.",
                    attribute.name
                ),
                ManifoldAttributeKind::TextExpression => format!(
                    "Expression `{}` is evaluated by Manifold because its ancestor is registered with `data-mf-register`.",
                    attribute.name
                ),
            };
                let mut message_parts = vec![intro_message];

                if let Some(type_info) = document.parsed.expression_type(&attribute) {
                    message_parts.push(format!("Type: {}", type_info.describe()));
                }

                if let Some(binding) = &attribute.loop_binding {
                    let mut binding_lines = Vec::new();
                    binding_lines.push(format!(
                        "Collection `{}`: {}",
                        binding.collection,
                        binding.collection_type.describe()
                    ));
                    if let Some(item) = &binding.item {
                        binding_lines.push(format!("Item `{}`: {}", item.name, item.ty.describe()));
                    }
                    if let Some(index) = &binding.index {
                        binding_lines.push(format!(
                            "Index `{}`: {}",
                            index.name,
                            index.ty.describe()
                        ));
                    }
                    message_parts.push(binding_lines.join("\n"));
                }

                let mut message = message_parts.join("\n\n");

                if let Some(expr) = attribute
                    .expression
                    .as_ref()
                    .map(|expr| expr.trim())
                    .filter(|expr| !expr.is_empty())
                {
                    if parse_expression(expr).is_ok() {
                        let sanitized = expr.replace("```", "`\u{200b}```");
                        message.push_str("\n\n```ts\n");
                        message.push_str(&sanitized);
                        message.push_str("\n```");
                    }
                }

                let hover = Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: message,
                    }),
                    range: Some(range),
                };
                return Ok(Some(hover));
            }
        }

        Ok(None)
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
