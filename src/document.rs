use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

use super::attribute::{ManifoldAttribute, ManifoldAttributeKind, ParsedAttribute};
use super::expression::{
    tokenize_expression, validate_expression_with_context, ExpressionToken, ExpressionTokenKind,
    ValidationContext,
};
use super::lineindex::LineIndex;
use std::collections::HashMap;

use super::state::{
    parse_states_from_document, resolve_identifier_type, LoopBinding, ManifoldStates,
    StateVariable, TypeInfo,
};

#[derive(Debug, Clone)]
pub struct ManifoldDocument {
    pub attributes: Vec<ManifoldAttribute>,
    pub line_index: LineIndex,
    pub states: ManifoldStates,
    pub text: String,
    pub base_dir: Option<std::path::PathBuf>,
}

#[derive(Debug, Clone)]
pub struct DocumentSemanticToken {
    pub line: u32,
    pub start_char: u32,
    pub length: u32,
    pub kind: ExpressionTokenKind,
}

impl ManifoldDocument {
    pub fn parse(text: &str) -> Self {
        let line_index = LineIndex::new(text);
        // Try to parse states including external imports and classic scripts.
        let (states, _unresolved) =
            parse_states_from_document(text, std::env::current_dir().ok().as_deref());
        let attributes = Self::extract_manifold_attributes(text, &line_index, &states);
        Self {
            attributes,
            line_index,
            states,
            text: text.to_string(),
            base_dir: std::env::current_dir().ok(),
        }
    }

    pub fn parse_with_base(text: &str, base_dir: Option<&std::path::Path>) -> Self {
        let line_index = LineIndex::new(text);
        let (states, _unresolved) = parse_states_from_document(text, base_dir);
        let attributes = Self::extract_manifold_attributes(text, &line_index, &states);
        Self {
            attributes,
            line_index,
            states,
            text: text.to_string(),
            base_dir: base_dir.map(|p| p.to_path_buf()),
        }
    }

    pub fn extract_manifold_attributes(
        text: &str,
        line_index: &LineIndex,
        states: &ManifoldStates,
    ) -> Vec<ManifoldAttribute> {
        TagScanner::new(text, line_index, states).collect_attributes()
    }

    pub fn manifold_attribute_at(&self, position: &Position) -> Option<&ManifoldAttribute> {
        let offset = self.line_index.offset_at(position)?;
        self.attributes
            .iter()
            .find(|attr| offset >= attr.start_offset && offset < attr.end_offset)
    }

    pub fn semantic_tokens(&self) -> Vec<DocumentSemanticToken> {
        let mut results = Vec::new();

        for attribute in &self.attributes {
            let (Some(expr), Some((span_start, _))) =
                (attribute.expression.as_ref(), attribute.expression_span)
            else {
                continue;
            };

            let tokens: Vec<ExpressionToken> = tokenize_expression(expr);
            let base_offset = span_start;

            for token in tokens {
                if token.end <= token.start {
                    continue;
                }

                let absolute_start = base_offset + token.start;
                let absolute_end = base_offset + token.end;
                let start_pos = self.line_index.position_at(absolute_start);
                let end_pos = self.line_index.position_at(absolute_end);

                if end_pos.line != start_pos.line {
                    continue;
                }

                let length = end_pos.character.saturating_sub(start_pos.character);

                if length == 0 {
                    continue;
                }

                results.push(DocumentSemanticToken {
                    line: start_pos.line,
                    start_char: start_pos.character,
                    length,
                    kind: token.kind,
                });
            }
        }

        results.sort_by(|a, b| (a.line, a.start_char).cmp(&(b.line, b.start_char)));
        results
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for attribute in &self.attributes {
            let (Some(expr), Some((start_offset, end_offset))) =
                (attribute.expression.as_ref(), attribute.expression_span)
            else {
                continue;
            };

            let allow_inline_functions = Self::attribute_allows_inline_functions(attribute);
            let allow_assignments = Self::attribute_allows_assignments(attribute);

            // Create validation context
            let context = ValidationContext {
                state_name: attribute.state_name.as_deref(),
                states: &self.states,
                locals: &attribute.locals,
            };

            // Skip variable reference validation for :each expressions since they have special syntax
            let skip_variable_validation = attribute.name.to_ascii_lowercase() == ":each"
                || attribute.name.to_ascii_lowercase() == "data-mf-each";

            if let Err(message) = validate_expression_with_context(
                expr,
                allow_assignments,
                allow_inline_functions,
                if skip_variable_validation {
                    None
                } else {
                    Some(&context)
                },
            ) {
                let range = Range::new(
                    self.line_index.position_at(start_offset),
                    self.line_index.position_at(end_offset),
                );

                diagnostics.push(Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("manifold".to_string()),
                    message,
                    related_information: None,
                    tags: None,
                    data: None,
                });
            }
        }

        // Surface a single hint diagnostic if there are unresolved external sources
        let (_states_check, unresolved) =
            parse_states_from_document(&self.text, self.base_dir.as_deref());
        if !unresolved.is_empty() {
            let msg = format!(
                "Some external Manifold state sources could not be read: {}. Configure include paths or ensure files exist.",
                unresolved.join(", ")
            );
            diagnostics.push(Diagnostic {
                range: Range::new(Position::new(0, 0), Position::new(0, 0)),
                severity: Some(DiagnosticSeverity::HINT),
                code: None,
                code_description: None,
                source: Some("manifold".to_string()),
                message: msg,
                related_information: None,
                tags: None,
                data: None,
            });
        }

        diagnostics
    }

    fn attribute_allows_inline_functions(attribute: &ManifoldAttribute) -> bool {
        if attribute.kind != ManifoldAttributeKind::Attribute {
            return false;
        }

        let lower = attribute.name.to_ascii_lowercase();
        lower.starts_with(":on") || lower.starts_with("data-mf-on")
    }

    fn attribute_allows_assignments(attribute: &ManifoldAttribute) -> bool {
        if attribute.kind != ManifoldAttributeKind::Attribute {
            return false;
        }

        let lower = attribute.name.to_ascii_lowercase();
        lower.starts_with(":on") || lower.starts_with("data-mf-on")
    }

    pub fn expression_type(&self, attribute: &ManifoldAttribute) -> Option<TypeInfo> {
        let expr = attribute.expression.as_ref()?.trim();
        if expr.is_empty() {
            return None;
        }

        let locals_map: HashMap<String, TypeInfo> = attribute
            .locals
            .iter()
            .map(|var| (var.name.clone(), var.ty.clone()))
            .collect();
        let state = attribute
            .state_name
            .as_ref()
            .and_then(|name| self.states.get(name));

        let lower_name = attribute.name.to_ascii_lowercase();
        if lower_name == ":each" {
            if let Some(binding) = &attribute.loop_binding {
                return Some(binding.collection_type.clone());
            }
        }

        let tokens = tokenize_expression(expr);
        if tokens.is_empty() {
            return None;
        }

        if tokens.iter().any(|token| {
            let text = &expr[token.start..token.end];
            matches!(
                text,
                "==" | "===" | "!==" | "!=" | ">" | "<" | ">=" | "<=" | "&&" | "||"
            )
        }) {
            return Some(TypeInfo::Boolean);
        }

        if let Some(first_ident) = tokens
            .iter()
            .find(|token| token.kind == ExpressionTokenKind::Identifier)
        {
            let identifier = expr[first_ident.start..first_ident.end].to_string();
            if let Some(ty) = resolve_identifier_type(&identifier, state, &locals_map) {
                return Some(ty);
            }
        }

        None
    }
}

struct TagScanner<'a> {
    text: &'a str,
    bytes: &'a [u8],
    line_index: &'a LineIndex,
    states: &'a ManifoldStates,
    idx: usize,
    stack: Vec<AncestorState>,
    results: Vec<ManifoldAttribute>,
}

#[derive(Debug, Clone)]
struct AncestorState {
    registered: bool,
    ignore_active: bool,
    state_name: Option<String>,
    locals: HashMap<String, TypeInfo>,
}

impl<'a> TagScanner<'a> {
    fn new(text: &'a str, line_index: &'a LineIndex, states: &'a ManifoldStates) -> Self {
        Self {
            text,
            bytes: text.as_bytes(),
            line_index,
            states,
            idx: 0,
            stack: Vec::new(),
            results: Vec::new(),
        }
    }

    fn collect_attributes(mut self) -> Vec<ManifoldAttribute> {
        let len = self.bytes.len();
        while self.idx < len {
            let Some(rel) = self.text[self.idx..].find('<') else {
                self.handle_text_node(self.idx, len);
                break;
            };
            let tag_start = self.idx + rel;
            self.handle_text_node(self.idx, tag_start);
            self.idx = tag_start + 1;

            if self.idx >= len {
                break;
            }

            let next_char = self.bytes[self.idx];
            match next_char {
                b'/' => {
                    self.idx += 1;
                    self.handle_closing_tag();
                }
                b'!' | b'?' => {
                    self.skip_special();
                }
                _ => {
                    self.handle_opening_tag(tag_start);
                }
            }
        }

        self.results
    }

    fn handle_closing_tag(&mut self) {
        let len = self.bytes.len();
        while self.idx < len {
            let ch = self.bytes[self.idx];
            if ch == b'>' {
                self.idx += 1;
                break;
            }
            self.idx += 1;
        }
        self.stack.pop();
    }

    fn handle_opening_tag(&mut self, tag_start: usize) {
        let len = self.bytes.len();
        let mut cursor = self.idx;
        let mut in_quote: Option<u8> = None;
        while cursor < len {
            let ch = self.bytes[cursor];
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
            self.idx = len;
            return;
        }

        let tag_end = cursor;
        self.idx = cursor + 1;

        let self_closing = self.is_self_closing(tag_start, tag_end);

        let mut attr_cursor = tag_start + 1;
        while attr_cursor < tag_end {
            let ch = self.bytes[attr_cursor];
            if ch.is_ascii_whitespace() {
                attr_cursor += 1;
                break;
            }
            if matches!(ch, b'/' | b'>') {
                break;
            }
            attr_cursor += 1;
        }

        let parent_state = self.current_state();
        let mut saw_register = false;
        let mut saw_ignore = false;
        let mut register_value_raw: Option<String> = None;
        let mut parsed_attributes: Vec<ParsedAttribute> = Vec::new();

        while attr_cursor < tag_end {
            while attr_cursor < tag_end && self.bytes[attr_cursor].is_ascii_whitespace() {
                attr_cursor += 1;
            }
            if attr_cursor >= tag_end {
                break;
            }
            if self.bytes[attr_cursor] == b'/' {
                attr_cursor += 1;
                continue;
            }

            let attr_name_start = attr_cursor;
            while attr_cursor < tag_end
                && !self.bytes[attr_cursor].is_ascii_whitespace()
                && !matches!(self.bytes[attr_cursor], b'=' | b'/')
            {
                attr_cursor += 1;
            }
            let attr_name_end = attr_cursor;
            if attr_name_start >= attr_name_end {
                break;
            }

            while attr_cursor < tag_end && self.bytes[attr_cursor].is_ascii_whitespace() {
                attr_cursor += 1;
            }

            let (attr_end, value_range) =
                if attr_cursor < tag_end && self.bytes[attr_cursor] == b'=' {
                    attr_cursor += 1;
                    while attr_cursor < tag_end && self.bytes[attr_cursor].is_ascii_whitespace() {
                        attr_cursor += 1;
                    }

                    if attr_cursor < tag_end {
                        if matches!(self.bytes[attr_cursor], b'"' | b'\'') {
                            let quote = self.bytes[attr_cursor];
                            attr_cursor += 1;
                            let value_start = attr_cursor;
                            while attr_cursor < tag_end && self.bytes[attr_cursor] != quote {
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
                                && !self.bytes[attr_cursor].is_ascii_whitespace()
                                && self.bytes[attr_cursor] != b'/'
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

            let name = self.text[attr_name_start..attr_name_end].to_string();
            let name_lower = name.to_ascii_lowercase();

            match name_lower.as_str() {
                "data-mf-register" => {
                    saw_register = true;
                    if let Some((value_start, value_end)) = value_range {
                        let raw_value = self.text[value_start..value_end].trim();
                        if !raw_value.is_empty() {
                            register_value_raw = Some(raw_value.to_string());
                        }
                    }
                }
                "data-mf-ignore" => {
                    saw_ignore = true;
                }
                _ => {
                    let name_range = Range::new(
                        self.line_index.position_at(attr_name_start),
                        self.line_index.position_at(attr_name_end),
                    );

                    parsed_attributes.push(ParsedAttribute {
                        name,
                        name_lower,
                        span_start: attr_name_start,
                        span_end: attr_end,
                        name_range,
                        value_range,
                    });
                }
            }
        }

        let mut new_state = parent_state.clone();
        if parent_state.ignore_active {
            new_state.registered = false;
            new_state.state_name = None;
        }

        if saw_ignore {
            new_state.registered = false;
            new_state.ignore_active = true;
            new_state.state_name = None;
            new_state.locals.clear();
        } else if saw_register {
            new_state.registered = true;
            new_state.ignore_active = false;
            let state_name = register_value_raw
                .clone()
                .filter(|value| !value.is_empty())
                .unwrap_or_else(|| "default".to_string());
            new_state.state_name = Some(state_name);
        }

        if new_state.registered && !new_state.ignore_active {
            let mut pending_locals: Vec<StateVariable> = Vec::new();

            for attr in parsed_attributes
                .iter()
                .filter(|attr| attr.name.starts_with(':') || attr.name_lower.starts_with("data-mf"))
            {
                let (start, end, expression, expression_span) = match attr.value_range {
                    Some((value_start, value_end)) if value_end > value_start => {
                        let raw = &self.text[value_start..value_end];
                        let trimmed = raw.trim();
                        if trimmed.is_empty() {
                            (value_start, value_end, None, None)
                        } else {
                            let rel_start = raw
                                .find(trimmed)
                                .map(|offset| value_start + offset)
                                .unwrap_or(value_start);
                            let rel_end = rel_start + trimmed.len();
                            (
                                value_start,
                                value_end,
                                Some(trimmed.to_string()),
                                Some((rel_start, rel_end)),
                            )
                        }
                    }
                    _ => (attr.span_start, attr.span_end, None, None),
                };

                let range = Range::new(
                    self.line_index.position_at(start),
                    self.line_index.position_at(end),
                );

                let locals_snapshot = new_state
                    .locals
                    .iter()
                    .map(|(name, ty)| StateVariable {
                        name: name.clone(),
                        ty: ty.clone(),
                    })
                    .collect::<Vec<_>>();
                let state_name_snapshot = new_state.state_name.clone();

                let mut loop_binding = None;
                if attr.name_lower == ":each" {
                    if let Some(expr_text) = expression.as_ref() {
                        loop_binding = self.parse_each_binding(
                            expr_text,
                            state_name_snapshot.as_deref(),
                            &new_state.locals,
                        );
                        if let Some(binding) = &loop_binding {
                            if let Some(item) = &binding.item {
                                pending_locals.push(item.clone());
                            }
                            if let Some(index) = &binding.index {
                                pending_locals.push(index.clone());
                            }
                        }
                    }
                }

                self.results.push(ManifoldAttribute {
                    name: attr.name.clone(),
                    name_range: attr.name_range.clone(),
                    range,
                    start_offset: start,
                    end_offset: end,
                    kind: ManifoldAttributeKind::Attribute,
                    expression,
                    expression_span,
                    state_name: state_name_snapshot,
                    locals: locals_snapshot,
                    loop_binding,
                });
            }

            for local in pending_locals {
                new_state
                    .locals
                    .insert(local.name.clone(), local.ty.clone());
            }
        }

        if !self_closing {
            self.stack.push(new_state);
        }
    }

    fn is_self_closing(&self, tag_start: usize, tag_end: usize) -> bool {
        if tag_end <= tag_start {
            return false;
        }
        let mut cursor = tag_end;
        while cursor > tag_start {
            let ch = self.bytes[cursor - 1];
            if ch.is_ascii_whitespace() {
                cursor -= 1;
                continue;
            }
            return ch == b'/';
        }
        false
    }

    fn current_state(&self) -> AncestorState {
        self.stack.last().cloned().unwrap_or(AncestorState {
            registered: false,
            ignore_active: false,
            state_name: None,
            locals: HashMap::new(),
        })
    }

    fn skip_special(&mut self) {
        let len = self.bytes.len();
        while self.idx < len {
            let ch = self.bytes[self.idx];
            if ch == b'>' {
                self.idx += 1;
                break;
            }
            self.idx += 1;
        }
    }

    fn handle_text_node(&mut self, start: usize, end: usize) {
        if start >= end {
            return;
        }
        let state = self.current_state();
        if !state.registered || state.ignore_active {
            return;
        }
        let mut search_idx = start;
        while search_idx < end {
            let remaining = &self.text[search_idx..end];
            let Some(rel) = remaining.find("${") else {
                break;
            };
            let expr_start = search_idx + rel;
            let mut cursor = expr_start + 2;
            let mut brace_depth = 0i32;
            while cursor < end {
                match self.bytes[cursor] {
                    b'{' => brace_depth += 1,
                    b'}' => {
                        if brace_depth == 0 {
                            cursor += 1;
                            break;
                        } else {
                            brace_depth -= 1;
                        }
                    }
                    _ => {}
                }
                cursor += 1;
            }
            if cursor > end {
                break;
            }
            let expr_end = cursor;
            let range = Range::new(
                self.line_index.position_at(expr_start),
                self.line_index.position_at(expr_end),
            );
            let name = self.text[expr_start..expr_end].to_string();
            let expression = if expr_end > expr_start + 3 {
                let raw = &self.text[expr_start + 2..expr_end - 1];
                let trimmed = raw.trim();
                if trimmed.is_empty() {
                    None
                } else {
                    Some(trimmed.to_string())
                }
            } else {
                None
            };
            let expression_span = expression.as_ref().map(|trimmed| {
                let raw = &self.text[expr_start + 2..expr_end - 1];
                let rel_start = raw
                    .find(trimmed)
                    .map(|offset| expr_start + 2 + offset)
                    .unwrap_or(expr_start + 2);
                let rel_end = rel_start + trimmed.len();
                (rel_start, rel_end)
            });
            let locals_snapshot = state
                .locals
                .iter()
                .map(|(name, ty)| StateVariable {
                    name: name.clone(),
                    ty: ty.clone(),
                })
                .collect::<Vec<_>>();
            self.results.push(ManifoldAttribute {
                name,
                name_range: range.clone(),
                range,
                start_offset: expr_start,
                end_offset: expr_end,
                kind: ManifoldAttributeKind::TextExpression,
                expression,
                expression_span,
                state_name: state.state_name.clone(),
                locals: locals_snapshot,
                loop_binding: None,
            });
            search_idx = expr_end;
        }
    }

    fn parse_each_binding(
        &self,
        expression: &str,
        state_name: Option<&str>,
        locals: &HashMap<String, TypeInfo>,
    ) -> Option<LoopBinding> {
        let mut parts = expression.splitn(2, " as ");
        let collection = parts.next()?.trim();
        if collection.is_empty() {
            return None;
        }
        let bindings = parts.next()?.trim();
        if bindings.is_empty() {
            return None;
        }

        let mut binding_parts = bindings
            .split(',')
            .map(|segment| segment.trim())
            .filter(|segment| !segment.is_empty());
        let item_name = binding_parts.next();
        let index_name = binding_parts.next();

        let state = state_name.and_then(|name| self.states.get(name));
        let collection_type =
            resolve_identifier_type(collection, state, locals).unwrap_or(TypeInfo::Any);
        let element_type = collection_type.element_type();

        let item = item_name.map(|name| StateVariable {
            name: name.to_string(),
            ty: element_type.clone(),
        });
        let index = index_name.map(|name| StateVariable {
            name: name.to_string(),
            ty: TypeInfo::Number,
        });

        Some(LoopBinding {
            collection: collection.to_string(),
            collection_type,
            item,
            index,
        })
    }
}
