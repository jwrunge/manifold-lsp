use crate::expression::{parse_expression, ExpressionTokenKind};
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use tower_lsp::{
    async_trait,
    jsonrpc::Error,
    lsp_types::{
        CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse,
        DidChangeConfigurationParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, ExecuteCommandOptions, ExecuteCommandParams,
        GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
        HoverProviderCapability, InitializeParams, InitializeResult, InlayHint, InlayHintParams,
        Location, MarkupContent, MarkupKind, MessageType, Position as LspPosition,
        Range as LspRange, ReferenceParams, SemanticToken, SemanticTokenType, SemanticTokens,
        SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
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

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
struct ManifoldConfig {
    #[serde(default)]
    files: FilePatterns,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct FilePatterns {
    #[serde(default = "default_include_patterns")]
    include: Vec<String>,
    #[serde(default = "default_exclude_patterns")]
    exclude: Vec<String>,
}

impl Default for FilePatterns {
    fn default() -> Self {
        Self {
            include: default_include_patterns(),
            exclude: default_exclude_patterns(),
        }
    }
}

fn default_include_patterns() -> Vec<String> {
    vec!["**/*.html".to_string(), "**/*.htm".to_string()]
}

fn default_exclude_patterns() -> Vec<String> {
    vec![
        "**/*.md".to_string(),
        "**/node_modules/**".to_string(),
        "**/.git/**".to_string(),
    ]
}

/// Simple glob pattern matching for file paths
/// Supports * (any characters except /) and ** (any characters including /)
fn matches_glob(path: &str, pattern: &str) -> bool {
    // Normalize path separators
    let path = path.replace('\\', "/");
    let pattern = pattern.replace('\\', "/");

    // Convert glob pattern to regex-like matching
    let pattern_parts: Vec<&str> = pattern.split('/').collect();
    let path_parts: Vec<&str> = path.split('/').collect();

    matches_glob_parts(&path_parts, &pattern_parts)
}

fn matches_glob_parts(path_parts: &[&str], pattern_parts: &[&str]) -> bool {
    if pattern_parts.is_empty() {
        return path_parts.is_empty();
    }

    if path_parts.is_empty() {
        return pattern_parts.iter().all(|p| *p == "**");
    }

    let pattern_head = pattern_parts[0];

    match pattern_head {
        "**" => {
            // ** can match zero or more path segments
            if pattern_parts.len() == 1 {
                // ** at the end matches everything
                return true;
            }

            // Try matching without consuming the ** (** matches zero segments)
            if matches_glob_parts(path_parts, &pattern_parts[1..]) {
                return true;
            }

            // Try matching with consuming one path segment (** matches one or more)
            matches_glob_parts(&path_parts[1..], pattern_parts)
        }
        _ => {
            let path_head = path_parts[0];
            // Match single segment with * support
            if matches_segment(path_head, pattern_head) {
                matches_glob_parts(&path_parts[1..], &pattern_parts[1..])
            } else {
                false
            }
        }
    }
}

fn matches_segment(segment: &str, pattern: &str) -> bool {
    if pattern == "*" {
        return true;
    }

    if !pattern.contains('*') {
        return segment == pattern;
    }

    let parts: Vec<&str> = pattern.split('*').collect();
    let mut pos = 0;

    for (i, part) in parts.iter().enumerate() {
        if part.is_empty() {
            continue;
        }

        if i == 0 {
            // First part must match at start
            if !segment.starts_with(part) {
                return false;
            }
            pos = part.len();
        } else if i == parts.len() - 1 {
            // Last part must match at end
            if !segment.ends_with(part) {
                return false;
            }
            if pos > segment.len() - part.len() {
                return false;
            }
        } else {
            // Middle parts must be found in order
            if let Some(found) = segment[pos..].find(part) {
                pos += found + part.len();
            } else {
                return false;
            }
        }
    }

    true
}

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

fn make_variable_completion(label: &str, detail: Option<String>) -> CompletionItem {
    CompletionItem {
        label: label.to_string(),
        kind: Some(CompletionItemKind::VARIABLE),
        detail,
        sort_text: Some(format!("0{label}")),
        ..Default::default()
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
    config: std::sync::RwLock<ManifoldConfig>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
            config: std::sync::RwLock::new(ManifoldConfig::default()),
        }
    }

    fn should_process_file(&self, uri: &Url) -> bool {
        let Some(path) = uri.to_file_path().ok() else {
            return true; // Process non-file URIs by default
        };

        let path_str = path.to_string_lossy();
        let config = self.config.read().unwrap();

        // Check exclude patterns first (they take precedence)
        for pattern in &config.files.exclude {
            if matches_glob(&path_str, pattern) {
                return false;
            }
        }

        // Then check include patterns
        for pattern in &config.files.include {
            if matches_glob(&path_str, pattern) {
                return true;
            }
        }

        // If no patterns match, default to false
        false
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
                                token_types: SEMANTIC_TOKEN_TYPES.to_vec(),
                                token_modifiers: Vec::new(),
                            },
                            range: None,
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec![String::from("custom.notification")],
                    work_done_progress_options: Default::default(),
                }),
                ..ServerCapabilities::default()
            },
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

        if let Some(attr) = doc.parsed.manifold_attribute_at(&position) {
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
            if let Some(ident) = property_identifier_at(attr, li, &position) {
                let state_name = attr
                    .state_name
                    .clone()
                    .unwrap_or_else(|| "default".to_string());

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
            }
        }

        drop(doc);

        let mut cross_document_results: Vec<Location> = Vec::new();
        for entry in self.documents.iter() {
            let html_uri = entry.key();
            let document = entry.value();
            let html_path = html_uri.to_file_path().ok();
            let (index, _unresolved) =
                crate::state::build_definition_index(&document.text, html_path.as_deref());

            for ((state_name, prop_name), location) in index.iter() {
                if location.uri != uri {
                    continue;
                }

                let start = LspPosition::new(location.line, location.character);
                let end = LspPosition::new(location.line, location.character + location.length);
                if !position_within_range(&position, &start, &end) {
                    continue;
                }

                for range in document.parsed.property_references(state_name, prop_name) {
                    cross_document_results.push(Location {
                        uri: html_uri.clone(),
                        range,
                    });
                }
            }
        }

        if !cross_document_results.is_empty() {
            return Ok(Some(GotoDefinitionResponse::Array(cross_document_results)));
        }

        Ok(None)
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> Result<Option<CompletionResponse>, Error> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        let Some(candidates) = doc.parsed.completion_candidates(&position) else {
            return Ok(None);
        };

        let mut items: Vec<CompletionItem> = candidates
            .iter()
            .map(|candidate| make_variable_completion(&candidate.label, candidate.detail.clone()))
            .collect();

        if let Some(first) = items.first_mut() {
            first.preselect = Some(true);
            first.sort_text = Some(String::from("!0"));
        }
        for (idx, item) in items.iter_mut().enumerate().skip(1) {
            item.sort_text = Some(format!("!{idx}{}", item.label));
        }

        let list = tower_lsp::lsp_types::CompletionList {
            is_incomplete: false,
            items,
        };

        Ok(Some(CompletionResponse::List(list)))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>, Error> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;

        if let Some(doc) = self.documents.get(&uri) {
            if let Some(attribute) = doc.parsed.manifold_attribute_at(&position) {
                if attribute.name.eq_ignore_ascii_case("data-mf-register") {
                    return Ok(Some(Vec::new()));
                }

                let mut results: Vec<Location> = Vec::new();
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

                return Ok(Some(results));
            }
        }

        let mut results: Vec<Location> = Vec::new();

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

        // Check if file should be processed based on patterns
        if !self.should_process_file(&uri) {
            return;
        }

        self.update_document(uri.clone(), params.text_document.text);
        self.refresh_diagnostics(uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let DidChangeTextDocumentParams {
            text_document,
            content_changes,
        } = params;

        let uri = text_document.uri;

        // Check if file should be processed based on patterns
        if !self.should_process_file(&uri) {
            return;
        }

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

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        // Extract the Manifold-specific configuration
        if let Some(settings) = params.settings.as_object() {
            if let Some(manifold_settings) = settings.get("manifoldLanguageServer") {
                match serde_json::from_value::<ManifoldConfig>(manifold_settings.clone()) {
                    Ok(config) => {
                        {
                            let mut current_config = self.config.write().unwrap();
                            *current_config = config;
                        } // Drop the write guard here

                        let _ = self
                            .client
                            .log_message(
                                MessageType::INFO,
                                "Manifold LSP configuration updated successfully",
                            )
                            .await;
                    }
                    Err(e) => {
                        let _ = self
                            .client
                            .log_message(
                                MessageType::WARNING,
                                format!("Failed to parse Manifold configuration: {e}"),
                            )
                            .await;
                    }
                }
            }
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>, Error> {
        let position_params = params.text_document_position_params;
        let uri = position_params.text_document.uri;
        let position = position_params.position;

        if let Some(document) = self.documents.get(&uri) {
            if let Some(attribute) = document.parsed.manifold_attribute_at(&position).cloned() {
                let range = attribute.range;
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
                    for item in &binding.items {
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
                    format!("Command executed successfully with params: {params:?}"),
                )
                .await;

            Ok(None)
        } else {
            Err(Error::invalid_request())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_glob_matching() {
        // Basic exact matches
        assert!(matches_glob("foo.html", "foo.html"));
        assert!(!matches_glob("bar.html", "foo.html"));

        // Single wildcard
        assert!(matches_glob("file.html", "*.html"));
        assert!(matches_glob("index.html", "*.html"));
        assert!(!matches_glob("file.md", "*.html"));

        // Directory wildcards
        assert!(matches_glob("src/file.html", "**/*.html"));
        assert!(matches_glob("a/b/c/file.html", "**/*.html"));
        assert!(matches_glob("file.html", "**/*.html"));

        // Specific directory patterns
        assert!(matches_glob(
            "node_modules/lib/file.js",
            "**/node_modules/**"
        ));
        assert!(matches_glob(
            "src/node_modules/file.js",
            "**/node_modules/**"
        ));
        assert!(!matches_glob("src/file.js", "**/node_modules/**"));

        // Extension patterns
        assert!(matches_glob("file.md", "**/*.md"));
        assert!(matches_glob("docs/readme.md", "**/*.md"));
        assert!(!matches_glob("file.html", "**/*.md"));

        // Multiple extensions
        assert!(matches_glob("file.astro", "**/*.astro"));
        assert!(matches_glob("src/components/Button.astro", "**/*.astro"));

        // Path with mixed separators (should normalize)
        assert!(matches_glob("src/file.html", "src/*.html"));
        assert!(matches_glob("src/nested/file.html", "src/**/*.html"));
    }

    #[test]
    fn test_segment_matching() {
        assert!(matches_segment("file", "file"));
        assert!(matches_segment("file", "*"));
        assert!(matches_segment("file.html", "*.html"));
        assert!(matches_segment("test.html", "*.html"));
        assert!(matches_segment("index.html", "index.*"));
        assert!(!matches_segment("file.md", "*.html"));
    }
}
