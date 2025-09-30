use std::collections::HashMap;

use dashmap::DashMap;
use log::debug;
use manifold_language_server::manifold_extract::{
    extract_manifold_content, ExtractType, ManifoldExtract,
};
use manifold_language_server::manifold_lang::ImCompleteSemanticToken;
use manifold_language_server::semantic_analyze::{IdentType, Semantic};
use manifold_language_server::semantic_token::LEGEND_TYPE;
use manifold_language_server::span::Span;
use ropey::Rope;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
#[derive(Debug)]
struct Backend {
    client: Client,
    manifold_extracts_map: DashMap<String, Vec<ManifoldExtract>>,
    semantic_map: DashMap<String, Semantic>,
    document_map: DashMap<String, Rope>,
    semantic_token_map: DashMap<String, Vec<ImCompleteSemanticToken>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            offset_encoding: None,
            capabilities: ServerCapabilities {
                inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),

                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![
                                        DocumentFilter {
                                            language: Some("html".to_string()),
                                            scheme: Some("file".to_string()),
                                            pattern: None,
                                        },
                                        DocumentFilter {
                                            language: Some("xml".to_string()),
                                            scheme: Some("file".to_string()),
                                            pattern: None,
                                        },
                                        DocumentFilter {
                                            language: None,
                                            scheme: Some("file".to_string()),
                                            pattern: Some("**/*.html".to_string()),
                                        },
                                    ]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                // definition: Some(GotoCapability::default()),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }
    async fn initialized(&self, _: InitializedParams) {
        debug!("initialized!");
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        debug!("file opened");
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: &params.text_document.text,
            version: Some(params.text_document.version),
        })
        .await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            text: &params.content_changes[0].text,
            uri: params.text_document.uri,
            version: Some(params.text_document.version),
        })
        .await
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        dbg!(&params.text);
        if let Some(text) = params.text {
            let item = TextDocumentItem {
                uri: params.text_document.uri,
                text: &text,
                version: None,
            };
            self.on_change(item).await;
            _ = self.client.semantic_tokens_refresh().await;
        }
        debug!("file saved!");
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        debug!("file closed!");
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let definition = || -> Option<GotoDefinitionResponse> {
            let uri = params.text_document_position_params.text_document.uri;
            let semantic = self.semantic_map.get(uri.as_str())?;
            let rope = self.document_map.get(uri.as_str())?;
            let position = params.text_document_position_params.position;
            let offset = position_to_offset(position, &rope)?;

            let interval = semantic.ident_range.find(offset, offset + 1).next()?;
            let interval_val = interval.val;
            let range = match interval_val {
                IdentType::Binding(symbol_id) => {
                    let span = &semantic.table.symbol_id_to_span[symbol_id];
                    Some(span.clone())
                }
                IdentType::Reference(reference_id) => {
                    let reference = semantic.table.reference_id_to_reference.get(reference_id)?;
                    let symbol_id = reference.symbol_id?;
                    let symbol_range = semantic.table.symbol_id_to_span.get(symbol_id)?;
                    Some(symbol_range.clone())
                }
            };

            range.and_then(|range| {
                let start_position = offset_to_position(range.start, &rope)?;
                let end_position = offset_to_position(range.end, &rope)?;
                Some(GotoDefinitionResponse::Scalar(Location::new(
                    uri,
                    Range::new(start_position, end_position),
                )))
            })
        }();
        Ok(definition)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let reference_list = || -> Option<Vec<Location>> {
            let uri = params.text_document_position.text_document.uri;
            let semantic = self.semantic_map.get(uri.as_str())?;
            let rope = self.document_map.get(uri.as_str())?;
            let position = params.text_document_position.position;
            let offset = position_to_offset(position, &rope)?;
            let reference_span_list = get_references(&semantic, offset, offset + 1, false)?;

            let ret = reference_span_list
                .into_iter()
                .filter_map(|range| {
                    let start_position = offset_to_position(range.start, &rope)?;
                    let end_position = offset_to_position(range.end, &rope)?;

                    let range = Range::new(start_position, end_position);

                    Some(Location::new(uri.clone(), range))
                })
                .collect::<Vec<_>>();
            Some(ret)
        }();
        Ok(reference_list)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        debug!("semantic_token_full");
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let mut im_complete_tokens = self.semantic_token_map.get_mut(&uri)?;
            let rope = self.document_map.get(&uri)?;
            im_complete_tokens.sort_by(|a, b| a.start.cmp(&b.start));
            let mut pre_line = 0;
            let mut pre_start = 0;
            let semantic_tokens = im_complete_tokens
                .iter()
                .filter_map(|token| {
                    let line = rope.try_byte_to_line(token.start).ok()? as u32;
                    let first = rope.try_line_to_char(line as usize).ok()? as u32;
                    let start = rope.try_byte_to_char(token.start).ok()? as u32 - first;
                    let delta_line = line - pre_line;
                    let delta_start = if delta_line == 0 {
                        start - pre_start
                    } else {
                        start
                    };
                    let ret = Some(SemanticToken {
                        delta_line,
                        delta_start,
                        length: token.length as u32,
                        token_type: token.token_type as u32,
                        token_modifiers_bitset: 0,
                    });
                    pre_line = line;
                    pre_start = start;
                    ret
                })
                .collect::<Vec<_>>();
            Some(semantic_tokens)
        }();
        if let Some(semantic_token) = semantic_tokens {
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            })));
        }
        Ok(None)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri.to_string();
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let im_complete_tokens = self.semantic_token_map.get(&uri)?;
            let rope = self.document_map.get(&uri)?;
            let mut pre_line = 0;
            let mut pre_start = 0;
            let semantic_tokens = im_complete_tokens
                .iter()
                .filter_map(|token| {
                    let line = rope.try_byte_to_line(token.start).ok()? as u32;
                    let first = rope.try_line_to_char(line as usize).ok()? as u32;
                    let start = rope.try_byte_to_char(token.start).ok()? as u32 - first;
                    let ret = Some(SemanticToken {
                        delta_line: line - pre_line,
                        delta_start: if start >= pre_start {
                            start - pre_start
                        } else {
                            start
                        },
                        length: token.length as u32,
                        token_type: token.token_type as u32,
                        token_modifiers_bitset: 0,
                    });
                    pre_line = line;
                    pre_start = start;
                    ret
                })
                .collect::<Vec<_>>();
            Some(semantic_tokens)
        }();
        Ok(semantic_tokens.map(|data| {
            SemanticTokensRangeResult::Tokens(SemanticTokens {
                result_id: None,
                data,
            })
        }))
    }

    async fn inlay_hint(
        &self,
        params: tower_lsp::lsp_types::InlayHintParams,
    ) -> Result<Option<Vec<InlayHint>>> {
        debug!("inlay hint");
        let uri = &params.text_document.uri;

        if let Some(manifold_extracts) = self.manifold_extracts_map.get(uri.as_str()) {
            // Provide type hints for Manifold expressions
            let mut hints = Vec::new();

            for extract in manifold_extracts.iter() {
                match &extract.extract_type {
                    ExtractType::Interpolation => {
                        // Add type hints for interpolation expressions
                        // For now, just show the expression type
                        hints.push(InlayHint {
                            position: extract.range.end,
                            label: InlayHintLabel::String(" (expr)".to_string()),
                            kind: Some(InlayHintKind::TYPE),
                            text_edits: None,
                            tooltip: Some(InlayHintTooltip::String(
                                "Manifold interpolation expression".to_string(),
                            )),
                            padding_left: Some(true),
                            padding_right: Some(false),
                            data: None,
                        });
                    }
                    ExtractType::Directive { attribute_name } => {
                        // Add type hints for directive expressions
                        if matches!(attribute_name.as_str(), "if" | "elif" | "show" | "hide") {
                            hints.push(InlayHint {
                                position: extract.range.end,
                                label: InlayHintLabel::String(" (boolean)".to_string()),
                                kind: Some(InlayHintKind::TYPE),
                                text_edits: None,
                                tooltip: Some(InlayHintTooltip::String(
                                    "Boolean expression".to_string(),
                                )),
                                padding_left: Some(true),
                                padding_right: Some(false),
                                data: None,
                            });
                        }
                    }
                    _ => {
                        // No hints for data attributes for now
                    }
                }
            }

            return Ok(Some(hints));
        }

        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let completions = || -> Option<Vec<CompletionItem>> {
            let rope = self.document_map.get(&uri.to_string())?;
            let manifold_extracts = self.manifold_extracts_map.get(&uri.to_string())?;

            let line_char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = line_char + position.character as usize;

            // Check if we're inside a Manifold extract
            for extract in manifold_extracts.iter() {
                let extract_start_offset = rope
                    .try_line_to_char(extract.range.start.line as usize)
                    .ok()?
                    + extract.range.start.character as usize;
                let extract_end_offset = rope
                    .try_line_to_char(extract.range.end.line as usize)
                    .ok()?
                    + extract.range.end.character as usize;

                if offset >= extract_start_offset && offset <= extract_end_offset {
                    // We're inside a Manifold extract - provide context-specific completions
                    return Some(get_manifold_completions(&extract.extract_type));
                }
            }

            // Check if we're in a position to add Manifold directives (within a tag)
            let line = rope.line(position.line as usize).to_string();
            if line.contains('<') && !line.trim_start().starts_with("</") {
                // We're potentially in a tag - provide directive completions
                return Some(get_directive_completions());
            }

            None
        };

        if let Some(items) = completions() {
            Ok(Some(CompletionResponse::Array(items)))
        } else {
            Ok(None)
        }
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let workspace_edit = || -> Option<WorkspaceEdit> {
            let uri = params.text_document_position.text_document.uri;
            let semantic = self.semantic_map.get(uri.as_str())?;
            let rope = self.document_map.get(uri.as_str())?;
            let position = params.text_document_position.position;
            let offset = position_to_offset(position, &rope)?;
            let reference_list = get_references(&semantic, offset, offset + 1, true)?;

            let new_name = params.new_name;
            (!reference_list.is_empty()).then_some(()).map(|_| {
                let edit_list = reference_list
                    .into_iter()
                    .filter_map(|range| {
                        let start_position = offset_to_position(range.start, &rope)?;
                        let end_position = offset_to_position(range.end, &rope)?;
                        Some(TextEdit::new(
                            Range::new(start_position, end_position),
                            new_name.clone(),
                        ))
                    })
                    .collect::<Vec<_>>();
                let mut map = HashMap::new();
                map.insert(uri, edit_list);
                WorkspaceEdit::new(map)
            })
        }();
        Ok(workspace_edit)
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        debug!("configuration changed!");
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        debug!("workspace folders changed!");
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        debug!("watched files have changed!");
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        debug!("command executed!");

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }
}
#[derive(Debug, Deserialize, Serialize)]
struct InlayHintParams {
    path: String,
}

#[allow(unused)]
enum CustomNotification {}
impl Notification for CustomNotification {
    type Params = InlayHintParams;
    const METHOD: &'static str = "custom/notification";
}
struct TextDocumentItem<'a> {
    uri: Url,
    text: &'a str,
    version: Option<i32>,
}

impl Backend {
    async fn on_change<'a>(&self, params: TextDocumentItem<'a>) {
        debug!("Processing document change for: {}", params.uri);
        let rope = ropey::Rope::from_str(params.text);
        self.document_map
            .insert(params.uri.to_string(), rope.clone());

        // Extract only Manifold-specific content instead of parsing the entire HTML
        let manifold_extracts = extract_manifold_content(params.text);
        self.manifold_extracts_map
            .insert(params.uri.to_string(), manifold_extracts.clone());

        // Only create diagnostics for Manifold-specific syntax errors, not HTML errors
        let mut diagnostics = Vec::new();

        // Validate each Manifold extract individually
        for extract in &manifold_extracts {
            match &extract.extract_type {
                ExtractType::Interpolation => {
                    // Validate JavaScript expression syntax in interpolations
                    if extract.content.trim().is_empty() {
                        diagnostics.push(Diagnostic::new_simple(
                            extract.range,
                            "Empty interpolation expression".to_string(),
                        ));
                    }
                    // Add more JavaScript expression validation here if needed
                }
                ExtractType::Directive { attribute_name } => {
                    // Validate directive-specific syntax
                    match attribute_name.as_str() {
                        "if" | "elif" => {
                            if extract.content.trim().is_empty() {
                                diagnostics.push(Diagnostic::new_simple(
                                    extract.range,
                                    "Conditional directive requires an expression".to_string(),
                                ));
                            }
                        }
                        "each" => {
                            if !extract.content.contains(" as ") {
                                diagnostics.push(Diagnostic::new_simple(
                                    extract.range,
                                    "Each directive requires 'items as item' syntax".to_string(),
                                ));
                            }
                        }
                        _ => {
                            // Other directives - basic validation
                            if extract.content.trim().is_empty()
                                && !matches!(attribute_name.as_str(), "else")
                            {
                                diagnostics.push(Diagnostic::new_simple(
                                    extract.range,
                                    format!("Directive '{}' requires a value", attribute_name),
                                ));
                            }
                        }
                    }
                }
                ExtractType::DataAttribute { attribute_name } => {
                    // Validate data-mf attributes
                    if attribute_name == "data-mf-register" && !extract.content.is_empty() {
                        // Validate context name if provided
                        if !extract
                            .content
                            .chars()
                            .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
                        {
                            diagnostics.push(Diagnostic::new_simple(
                                extract.range,
                                "Context name should contain only alphanumeric characters, underscores, and hyphens".to_string(),
                            ));
                        }
                    }
                }
            }
        }

        // Send only Manifold-specific diagnostics
        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, params.version)
            .await;

        debug!(
            "Document processing complete. Found {} Manifold extracts",
            manifold_extracts.len()
        );
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        manifold_extracts_map: DashMap::new(),
        document_map: DashMap::new(),
        semantic_token_map: DashMap::new(),
        semantic_map: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

fn get_manifold_completions(extract_type: &ExtractType) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    match extract_type {
        ExtractType::Interpolation => {
            // Provide JavaScript expression completions
            completions.extend([
                CompletionItem {
                    label: "console.log".to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some("Console logging function".to_string()),
                    insert_text: Some("console.log($0)".to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                },
                CompletionItem {
                    label: "length".to_string(),
                    kind: Some(CompletionItemKind::PROPERTY),
                    detail: Some("Array/string length property".to_string()),
                    ..Default::default()
                },
            ]);
        }
        ExtractType::Directive { .. } => {
            // Provide common directive expressions
            completions.extend([
                CompletionItem {
                    label: "true".to_string(),
                    kind: Some(CompletionItemKind::VALUE),
                    detail: Some("Boolean true".to_string()),
                    ..Default::default()
                },
                CompletionItem {
                    label: "false".to_string(),
                    kind: Some(CompletionItemKind::VALUE),
                    detail: Some("Boolean false".to_string()),
                    ..Default::default()
                },
            ]);
        }
        ExtractType::DataAttribute { .. } => {
            // No specific completions for data attributes yet
        }
    }

    completions
}

fn get_directive_completions() -> Vec<CompletionItem> {
    vec![
        CompletionItem {
            label: ":if".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Conditional rendering directive".to_string()),
            insert_text: Some(":if=\"$0\"".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: ":each".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("List iteration directive".to_string()),
            insert_text: Some(":each=\"${1:items} as ${2:item}$0\"".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: ":onclick".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Click event handler".to_string()),
            insert_text: Some(":onclick=\"$0\"".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: ":sync:value".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Two-way value binding".to_string()),
            insert_text: Some(":sync:value=\"$0\"".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "data-mf-register".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Register Manifold context".to_string()),
            insert_text: Some("data-mf-register=\"$0\"".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "data-mf-ignore".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Ignore from Manifold processing".to_string()),
            ..Default::default()
        },
    ]
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char_of_line = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char_of_line;
    Some(Position::new(line as u32, column as u32))
}

fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
    let line_char_offset = rope.try_line_to_char(position.line as usize).ok()?;
    let slice = rope.slice(0..line_char_offset + position.character as usize);
    Some(slice.len_bytes())
}

fn get_references(
    semantic: &Semantic,
    start: usize,
    end: usize,
    include_definition: bool,
) -> Option<Vec<Span>> {
    let interval = semantic.ident_range.find(start, end).next()?;
    let interval_val = interval.val;
    match interval_val {
        IdentType::Binding(symbol_id) => {
            let references = semantic.table.symbol_id_to_references.get(&symbol_id)?;
            let mut reference_span_list: Vec<Span> = references
                .iter()
                .map(|reference_id| {
                    semantic.table.reference_id_to_reference[*reference_id]
                        .span
                        .clone()
                })
                .collect();
            if include_definition {
                let symbol_range = semantic.table.symbol_id_to_span.get(symbol_id)?;
                reference_span_list.push(symbol_range.clone());
            }
            Some(reference_span_list)
        }
        IdentType::Reference(_) => None,
    }
}
