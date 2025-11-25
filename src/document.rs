use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, InlayHint, InlayHintKind, InlayHintLabel, Position, Range,
};

use super::attribute::{ManifoldAttribute, ManifoldAttributeKind, ParsedAttribute};
use super::expression::{
    parse_expression_ast, tokenize_expression, validate_expression_with_context, ExpressionToken,
    ExpressionTokenKind, ValidationContext,
};
use super::lineindex::LineIndex;
use std::collections::{HashMap, HashSet};
use swc_ecma_ast::{Expr, Lit};

use super::state::{
    parse_states_from_document, resolve_identifier_type, resolve_member_type, LoopBinding,
    ManifoldStates, StateVariable, TypeInfo,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionCandidate {
    pub label: String,
    pub detail: Option<String>,
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
            .find(|attr| offset >= attr.start_offset && offset <= attr.end_offset)
    }

    pub fn completion_candidates(&self, position: &Position) -> Option<Vec<CompletionCandidate>> {
        let attribute = self.manifold_attribute_at(position)?;
        let offset = self.line_index.offset_at(position)?;

        if !attribute_contains_offset(attribute, offset) {
            return None;
        }

        let mut seen = HashSet::new();
        let mut candidates: Vec<CompletionCandidate> = Vec::new();

        let state_name = attribute.state_name.as_deref().unwrap_or("default");

        if let Some(state) = self.states.get(state_name) {
            for (name, ty) in state.properties.iter() {
                if seen.insert(name.clone()) {
                    candidates.push(CompletionCandidate {
                        label: name.clone(),
                        detail: Some(ty.describe()),
                    });
                }
            }
        }

        for local in &attribute.locals {
            if seen.insert(local.name.clone()) {
                candidates.push(CompletionCandidate {
                    label: local.name.clone(),
                    detail: Some(local.ty.describe()),
                });
            }
        }

        if candidates.is_empty() {
            return None;
        }

        candidates.sort_by(|a, b| a.label.cmp(&b.label));
        Some(candidates)
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

    pub fn inlay_hints(&self, range: &Range) -> Vec<InlayHint> {
        let range_start = self.line_index.offset_at(&range.start).unwrap_or(0);
        let range_end = self
            .line_index
            .offset_at(&range.end)
            .unwrap_or(self.text.len());

        let mut hints = Vec::new();
        let mut seen = HashSet::new();

        for attribute in &self.attributes {
            let Some((span_start, span_end)) = attribute.expression_span else {
                continue;
            };

            let lower_name = attribute.name.to_ascii_lowercase();
            if lower_name.starts_with(":on") || lower_name.starts_with("data-mf-on") {
                continue;
            }

            if span_end < range_start || span_start > range_end {
                continue;
            }

            if let Some(type_info) = self.expression_type(attribute) {
                if matches!(type_info, TypeInfo::Any) {
                    continue;
                }
                let label_text = format!(": {}", type_info.describe());
                let position = self.line_index.position_at(span_end);

                if !seen.insert((position.line, position.character, label_text.clone())) {
                    continue;
                }

                let label: InlayHintLabel = label_text.into();
                hints.push(InlayHint {
                    position,
                    label,
                    kind: Some(InlayHintKind::TYPE),
                    text_edits: None,
                    tooltip: None,
                    padding_left: Some(true),
                    padding_right: None,
                    data: None,
                });
            }
        }

        hints
    }

    pub fn property_references(&self, state_name: &str, property: &str) -> Vec<Range> {
        let mut ranges = Vec::new();

        for attribute in &self.attributes {
            let attr_state = attribute.state_name.as_deref().unwrap_or("default");
            if attr_state != state_name {
                continue;
            }

            let (Some(expr), Some((span_start, _span_end))) =
                (attribute.expression.as_ref(), attribute.expression_span)
            else {
                continue;
            };

            let expr_bytes = expr.as_bytes();
            let mut cursor = 0;
            while cursor <= expr.len().saturating_sub(property.len()) {
                let remaining = &expr[cursor..];
                let Some(rel_index) = remaining.find(property) else {
                    break;
                };

                let start = cursor + rel_index;
                let end = start + property.len();

                let prev_ok = if start == 0 {
                    true
                } else {
                    let ch = expr_bytes[start - 1];
                    !Self::is_identifier_char(ch)
                };

                let next_ok = if end >= expr.len() {
                    true
                } else {
                    let ch = expr_bytes[end];
                    !Self::is_identifier_char(ch)
                };

                if prev_ok && next_ok {
                    let absolute_start = span_start + start;
                    let absolute_end = span_start + end;
                    ranges.push(Range::new(
                        self.line_index.position_at(absolute_start),
                        self.line_index.position_at(absolute_end),
                    ));
                }

                cursor = start + 1;
            }
        }

        ranges
    }

    fn is_identifier_char(ch: u8) -> bool {
        ch.is_ascii_alphanumeric() || ch == b'_' || ch == b'$'
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
            let skip_variable_validation = attribute.name.eq_ignore_ascii_case(":each")
                || attribute.name.eq_ignore_ascii_case("data-mf-each")
                || attribute.name.eq_ignore_ascii_case(":then")
                || attribute.name.eq_ignore_ascii_case("data-mf-then")
                || attribute.name.eq_ignore_ascii_case(":catch")
                || attribute.name.eq_ignore_ascii_case("data-mf-catch");

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

        if let Some(identifier) = extract_identifier_chain(expr, &tokens) {
            if let Some(ty) = resolve_identifier_type(&identifier, state, &locals_map) {
                return Some(ty);
            }
        }

        None
    }
}

fn attribute_contains_offset(attribute: &ManifoldAttribute, offset: usize) -> bool {
    if let Some((start, end)) = attribute.expression_span {
        if offset >= start && offset <= end {
            return true;
        }
    }

    if let Some((start, end)) = attribute.value_range {
        if offset >= start && offset <= end {
            return true;
        }
    }

    offset >= attribute.start_offset && offset <= attribute.end_offset
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower_lsp::lsp_types::{DiagnosticSeverity, Position, Range};

    const SAMPLE_HTML: &str = r#"
        <div data-mf-register>
            <button :onclick="count++">Increment</button>
            <button :if="count > 5">${count}</button>
            <ul>
                <li :each="items as item, index">
                    Item: ${item} @ ${index} / ${count}
                </li>
            </ul>
        </div>
        <script type="module">
            const state = Manifold.create()
                .add("count", 0)
                .add("items", ["A", "B"])
                .add("status", "idle")
                .build();
        </script>
    "#;

    const ADVANCED_HTML: &str = r#"
        <div data-mf-register>
            <button :onclick=""></button>
            <div :await="currentUserPromise">Loading...</div>
            <div :then="{ name, age }">
                User: ${name} (${age})
            </div>
            <div :catch="errorMsg">
                Error: ${errorMsg}
            </div>
            <ul>
                <li :each="someArray as { name, age }, idx">
                    ${name} (${age}) #${idx}
                </li>
            </ul>
        </div>
        <script type="module">
            const state = Manifold.create()
                .add("currentUserPromise", null as Promise<unknown> | null)
                .add("someArray", [{ name: "Ada", age: 37 }])
                .build();
        </script>
    "#;

    const COLLECTION_EACH_HTML: &str = r#"
        <div data-mf-register>
            <ul>
                <li :each="setEntries as [setValue, setIndex]">
                    ${setValue}-${setIndex}
                </li>
                <li :each="mapEntries as mapValue, mapKey">
                    ${mapValue.label}-${mapKey.toUpperCase()}
                </li>
                <li :each="mapEntries as [entry, tupleKey]">
                    ${entry.label}-${tupleKey}
                </li>
                <li :each="recordEntries as { active }, recordKey">
                    ${recordKey}-${active}
                </li>
            </ul>
        </div>
        <script type="module">
            const state = Manifold.create()
                .add("setEntries", null as Set<number>)
                .add("mapEntries", null as Map<string, { label: string }>)
                .add("recordEntries", {
                    first: { active: true },
                    second: { active: false },
                })
                .build();
        </script>
    "#;

    fn position_in(doc: &ManifoldDocument, needle: &str, relative: usize) -> Position {
        let start = doc
            .text
            .find(needle)
            .unwrap_or_else(|| panic!("needle '{needle}' not found"));
        doc.line_index.position_at(start + relative)
    }

    #[test]
    fn completions_include_state_properties_inside_attribute() {
        let document = ManifoldDocument::parse(SAMPLE_HTML);
        let position = position_in(&document, "count++", 1);

        let completions = document
            .completion_candidates(&position)
            .expect("expected attribute completions");
        let labels: Vec<_> = completions.into_iter().map(|c| c.label).collect();
        assert!(labels.contains(&"count".to_string()));
        assert!(labels.contains(&"items".to_string()));
        assert!(labels.contains(&"status".to_string()));
    }

    #[test]
    fn completions_include_loop_locals_in_text_expression() {
        let document = ManifoldDocument::parse(SAMPLE_HTML);
        let position = position_in(&document, "${item}", 3);

        let completions = document
            .completion_candidates(&position)
            .expect("expected text expression completions");
        let labels: Vec<_> = completions.into_iter().map(|c| c.label).collect();
        assert!(labels.contains(&"item".to_string()));
        assert!(labels.contains(&"index".to_string()));
        assert!(labels.contains(&"count".to_string()));
    }

    #[test]
    fn attribute_completion_available_when_value_is_empty() {
        let html = r#"
            <div data-mf-register>
                <button :if=""></button>
            </div>
            <script type="module">
                const state = Manifold.create().add("count", 0).build();
            </script>
        "#;
        let document = ManifoldDocument::parse(html);
        let position = position_in(&document, ":if=\"\"", 5);

        let completions = document
            .completion_candidates(&position)
            .expect("expected completions for empty attribute value");
        let labels: Vec<_> = completions.into_iter().map(|c| c.label).collect();
        assert!(labels.contains(&"count".to_string()));
    }

    #[test]
    fn property_references_include_loop_and_text_usages() {
        let document = ManifoldDocument::parse(SAMPLE_HTML);
        let references = document.property_references("default", "count");
        assert!(references.len() >= 3);
    }

    #[test]
    fn inlay_hints_present_for_registered_attributes() {
        let document = ManifoldDocument::parse(SAMPLE_HTML);
        let end = document.line_index.position_at(document.text.len());
        let hints = document.inlay_hints(&Range::new(Position::new(0, 0), end));
        assert!(!hints.is_empty());
    }

    #[test]
    fn manifold_attribute_lookup_is_inclusive() {
        let document = ManifoldDocument::parse(SAMPLE_HTML);
        let end_position = position_in(&document, "count > 5\"", 0);
        let attribute = document
            .manifold_attribute_at(&end_position)
            .expect("should detect attribute at closing quote");
        assert_eq!(attribute.name, ":if".to_string());
    }

    #[test]
    fn await_attribute_infers_promise_type() {
        let document = ManifoldDocument::parse(ADVANCED_HTML);
        let await_attr = document
            .attributes
            .iter()
            .find(|attr| attr.name.eq_ignore_ascii_case(":await"))
            .expect(":await attribute present");
        let ty = document
            .expression_type(await_attr)
            .expect(":await attribute should have a type");
        assert_eq!(ty.describe(), "Promise<unknown> | null");
    }

    #[test]
    fn then_destructuring_binds_variables() {
        let document = ManifoldDocument::parse(ADVANCED_HTML);
        let position = position_in(&document, "${name}", 3);
        let completions = document
            .completion_candidates(&position)
            .expect("expected completions in :then block");
        let labels: Vec<_> = completions.into_iter().map(|c| c.label).collect();
        assert!(labels.contains(&"name".to_string()));
        assert!(labels.contains(&"age".to_string()));
    }

    #[test]
    fn catch_block_exposes_error_variable() {
        let document = ManifoldDocument::parse(ADVANCED_HTML);
        let position = position_in(&document, "${errorMsg}", 3);
        let completions = document
            .completion_candidates(&position)
            .expect("expected completions in :catch block");
        let labels: Vec<_> = completions.into_iter().map(|c| c.label).collect();
        assert!(labels.contains(&"errorMsg".to_string()));
    }

    #[test]
    fn each_destructuring_adds_loop_locals() {
        let document = ManifoldDocument::parse(ADVANCED_HTML);
        let position = position_in(&document, "${name} (${age}) #${idx}", 3);
        let completions = document
            .completion_candidates(&position)
            .expect("expected completions in :each block");
        let labels: Vec<_> = completions.into_iter().map(|c| c.label).collect();
        assert!(labels.contains(&"name".to_string()));
        assert!(labels.contains(&"age".to_string()));
        assert!(labels.contains(&"idx".to_string()));
    }

    #[test]
    fn each_literal_array_infers_string_items() {
        let html = r#"
            <div data-mf-register>
                <ul>
                    <li :each="['Error A', 'Error B'] as msg, idx">${msg}</li>
                </ul>
            </div>
        "#;
        let document = ManifoldDocument::parse(html);
        let position = position_in(&document, "${msg}", 3);
        let completions = document
            .completion_candidates(&position)
            .expect("expected completions in literal each block");
        let msg = completions
            .iter()
            .find(|c| c.label == "msg")
            .expect("msg completion present");
        assert_eq!(msg.detail.as_deref(), Some("string"));
    }

    #[test]
    fn each_infers_types_for_sets_maps_and_objects() {
        let document = ManifoldDocument::parse(COLLECTION_EACH_HTML);

        fn expect_detail(
            document: &ManifoldDocument,
            needle: &str,
            relative: usize,
            label: &str,
            expected: &str,
        ) {
            let position = position_in(document, needle, relative);
            let completions = document
                .completion_candidates(&position)
                .expect("expected completions in :each block");
            let detail = completions
                .iter()
                .find(|c| c.label == label)
                .unwrap_or_else(|| panic!("missing completion for {}", label));
            assert_eq!(detail.detail.as_deref(), Some(expected));
        }

        expect_detail(&document, "${setValue}", 3, "setValue", "number");
        expect_detail(&document, "${setIndex}", 3, "setIndex", "number");
        expect_detail(
            &document,
            "${mapValue.label}",
            3,
            "mapValue",
            "{ label: string }",
        );
        expect_detail(&document, "${mapKey.toUpperCase()}", 3, "mapKey", "string");
        expect_detail(&document, "${entry.label}", 3, "entry", "{ label: string }");
        expect_detail(&document, "${tupleKey}", 3, "tupleKey", "string");
        expect_detail(&document, "${recordKey}-${active}", 3, "recordKey", "string");
        expect_detail(&document, "-${active}", 3, "active", "boolean");
    }

    #[test]
    fn dollar_helper_available_in_attributes() {
        let document = ManifoldDocument::parse(ADVANCED_HTML);
        let onclick_attr = document
            .attributes
            .iter()
            .find(|attr| attr.name.eq_ignore_ascii_case(":onclick"))
            .expect(":onclick attribute present");
        let offset = onclick_attr
            .expression_span
            .map(|(start, _)| start)
            .unwrap_or(onclick_attr.start_offset);
        let position = document.line_index.position_at(offset);
        let completions = document
            .completion_candidates(&position)
            .expect("expected attribute completions");
        let labels: Vec<_> = completions.into_iter().map(|c| c.label).collect();
        assert!(labels.contains(&"$".to_string()));
    }

    #[test]
    fn example_document_has_no_unknown_variable_diagnostics() {
        let html = include_str!("../examples/index.html");
        let base_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples");
        let document = ManifoldDocument::parse_with_base(html, Some(base_dir.as_path()));
        let diagnostics = document.diagnostics();
        let errors: Vec<_> = diagnostics
            .into_iter()
            .filter(|d| d.severity == Some(DiagnosticSeverity::ERROR))
            .collect();
        assert!(errors.is_empty(), "unexpected diagnostics: {errors:?}");
    }
}

fn extract_identifier_chain(expr: &str, tokens: &[ExpressionToken]) -> Option<String> {
    let first_index = tokens
        .iter()
        .position(|token| token.kind == ExpressionTokenKind::Identifier)?;

    let first = &tokens[first_index];
    let mut chain = expr[first.start..first.end].to_string();
    let mut index = first_index + 1;

    while index + 1 < tokens.len() {
        let operator = &tokens[index];
        if operator.kind != ExpressionTokenKind::Operator {
            break;
        }

        let operator_text = expr[operator.start..operator.end].trim();
        if operator_text != "." {
            break;
        }

        let next = &tokens[index + 1];
        if next.kind != ExpressionTokenKind::Identifier {
            break;
        }

        chain.push('.');
        chain.push_str(expr[next.start..next.end].trim());
        index += 2;
    }

    Some(chain)
}

struct TagScanner<'a> {
    text: &'a str,
    bytes: &'a [u8],
    line_index: &'a LineIndex,
    states: &'a ManifoldStates,
    idx: usize,
    stack: Vec<AncestorState>,
    results: Vec<ManifoldAttribute>,
    pending_await: Option<AwaitContext>,
}

#[derive(Debug, Clone)]
struct AncestorState {
    registered: bool,
    ignore_active: bool,
    state_name: Option<String>,
    locals: HashMap<String, TypeInfo>,
}

#[derive(Debug, Clone)]
struct AwaitContext {
    promise_type: TypeInfo,
    depth: usize,
    consumed_then: bool,
    consumed_catch: bool,
}

impl AwaitContext {
    fn new(promise_type: TypeInfo, depth: usize) -> Self {
        Self {
            promise_type,
            depth,
            consumed_then: false,
            consumed_catch: false,
        }
    }
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
            pending_await: None,
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
        if let Some(context) = &self.pending_await {
            if context.depth > self.stack.len() {
                self.pending_await = None;
            }
        }
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
                    parsed_attributes.push(ParsedAttribute {
                        name,
                        name_lower,
                        span_start: attr_name_start,
                        span_end: attr_end,
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
            TagScanner::ensure_global_helpers(&mut new_state.locals);
            let mut pending_locals: Vec<StateVariable> = Vec::new();

            for attr in parsed_attributes
                .iter()
                .filter(|attr| attr.name.starts_with(':') || attr.name_lower.starts_with("data-mf"))
            {
                let attr_lower = attr.name_lower.as_str();
                if attr_lower == ":transition" || attr_lower == "data-mf-transition" {
                    continue;
                }
                let (start, end, expression, expression_span) = match attr.value_range {
                    Some((value_start, value_end)) if value_end > value_start => {
                        let raw = &self.text[value_start..value_end];
                        let trimmed = raw.trim();

                        if trimmed.is_empty() {
                            (
                                value_start,
                                value_end,
                                None,
                                Some((value_start, value_start)),
                            )
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
                    Some((value_start, value_end)) => (
                        value_start,
                        value_end,
                        None,
                        Some((value_start, value_start)),
                    ),
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
                if attr_lower == ":each" || attr_lower == "data-mf-each" {
                    if let Some(expr_text) = expression.as_ref() {
                        loop_binding = self.parse_each_binding(
                            expr_text,
                            state_name_snapshot.as_deref(),
                            &new_state.locals,
                        );
                        if let Some(binding) = &loop_binding {
                            for item in &binding.items {
                                pending_locals.push(item.clone());
                            }
                            if let Some(index) = &binding.index {
                                pending_locals.push(index.clone());
                            }
                        }
                    }
                }

                if attr_lower == ":await" || attr_lower == "data-mf-await" {
                    let promise_type = expression
                        .as_ref()
                        .and_then(|expr_text| {
                            TagScanner::parse_identifier_chain(expr_text.as_str())
                        })
                        .and_then(|identifier| {
                            let state = state_name_snapshot
                                .as_deref()
                                .and_then(|name| self.states.get(name));
                            resolve_identifier_type(&identifier, state, &new_state.locals)
                        })
                        .unwrap_or(TypeInfo::Any);
                    self.pending_await = Some(AwaitContext::new(promise_type, self.stack.len()));
                }

                if attr_lower == ":then" || attr_lower == "data-mf-then" {
                    if let Some(expr_text) = expression.as_ref() {
                        if let Some(context) = self.pending_await.as_mut() {
                            if context.depth == self.stack.len() {
                                let resolved = context.promise_type.promise_resolution_type();
                                let fallback = TypeInfo::Any;
                                let bindings = TagScanner::extract_pattern_bindings(
                                    expr_text,
                                    resolved.as_ref(),
                                    &fallback,
                                );
                                if !bindings.is_empty() {
                                    pending_locals.extend(bindings.into_iter());
                                }
                                context.consumed_then = true;
                            }
                        }
                    }
                }

                if attr_lower == ":catch" || attr_lower == "data-mf-catch" {
                    if let Some(expr_text) = expression.as_ref() {
                        if let Some(context) = self.pending_await.as_mut() {
                            if context.depth == self.stack.len() {
                                let fallback = TypeInfo::Named("Error".to_string());
                                let bindings = TagScanner::extract_pattern_bindings(
                                    expr_text,
                                    Some(&fallback),
                                    &fallback,
                                );
                                if !bindings.is_empty() {
                                    pending_locals.extend(bindings.into_iter());
                                }
                                context.consumed_catch = true;
                            }
                        }
                    }
                }

                if matches!(
                    attr_lower,
                    ":then" | "data-mf-then" | ":catch" | "data-mf-catch"
                ) {
                    let should_clear = self
                        .pending_await
                        .as_ref()
                        .map(|ctx| {
                            ctx.depth == self.stack.len() && ctx.consumed_then && ctx.consumed_catch
                        })
                        .unwrap_or(false);
                    if should_clear {
                        self.pending_await = None;
                    }
                }

                self.results.push(ManifoldAttribute {
                    name: attr.name.clone(),
                    range,
                    start_offset: start,
                    end_offset: end,
                    value_range: attr.value_range,
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
            let mut found_closing = false;
            while cursor < end {
                match self.bytes[cursor] {
                    b'{' => brace_depth += 1,
                    b'}' => {
                        if brace_depth == 0 {
                            cursor += 1;
                            found_closing = true;
                            break;
                        } else {
                            brace_depth -= 1;
                        }
                    }
                    _ => {}
                }
                cursor += 1;
            }
            if !found_closing {
                cursor = end;
            }
            let expr_end = cursor;
            let range = Range::new(
                self.line_index.position_at(expr_start),
                self.line_index.position_at(expr_end),
            );
            let name = self.text[expr_start..expr_end].to_string();
            let raw_slice = if found_closing {
                &self.text[expr_start + 2..expr_end - 1]
            } else {
                &self.text[expr_start + 2..expr_end]
            };
            let trimmed = raw_slice.trim();
            let expression = if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_string())
            };
            let expression_span = if let Some(expr_text) = expression.as_ref() {
                let rel_start = raw_slice
                    .find(expr_text)
                    .map(|offset| expr_start + 2 + offset)
                    .unwrap_or(expr_start + 2);
                let rel_end = rel_start + expr_text.len();
                Some((rel_start, rel_end))
            } else if found_closing {
                Some((expr_start + 2, expr_end.saturating_sub(1)))
            } else {
                Some((expr_start + 2, expr_end))
            };
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
                range,
                start_offset: expr_start,
                end_offset: expr_end,
                value_range: Some((expr_start + 2, expr_end)),
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

        let (item_part, key_part) = Self::split_each_bindings(bindings);
        if item_part.is_empty() {
            return None;
        }

        let state = state_name.and_then(|name| self.states.get(name));
        let collection_type = TagScanner::infer_collection_literal_type(collection)
            .or_else(|| resolve_identifier_type(collection, state, locals))
            .unwrap_or(TypeInfo::Any);
        let mut element_type = collection_type.element_type();
        let value_type = Self::infer_each_value_type(&collection_type);
        let mut binding_type = value_type.clone();
        if Self::is_array_pattern(item_part) {
            if !matches!(element_type, TypeInfo::Tuple(_)) {
                let index_ty = Self::infer_each_index_type(&collection_type);
                element_type = TypeInfo::Tuple(vec![value_type.clone(), index_ty]);
            }
            binding_type = element_type.clone();
        }

        let fallback_any = TypeInfo::Any;
        let items = Self::extract_pattern_bindings(
            item_part,
            Some(&binding_type),
            &fallback_any,
        );

        if items.is_empty() {
            return None;
        }

        let index = key_part.and_then(|name| {
            let trimmed = name.trim();
            if trimmed.is_empty() || !Self::is_valid_identifier(trimmed) {
                None
            } else {
                let index_ty = Self::infer_each_index_type(&collection_type);
                Some(StateVariable {
                    name: trimmed.to_string(),
                    ty: index_ty,
                })
            }
        });

        Some(LoopBinding {
            collection: collection.to_string(),
            collection_type,
            items,
            index,
        })
    }

    fn infer_collection_literal_type(expression: &str) -> Option<TypeInfo> {
        let ast = parse_expression_ast(expression).ok()?;
        match ast.as_ref() {
            Expr::Array(array) => {
                let mut element_types: Vec<TypeInfo> = Vec::new();
                for elem in array.elems.iter().flatten() {
                    if elem.spread.is_some() {
                        return None;
                    }
                    let ty = TagScanner::infer_literal_value_type(elem.expr.as_ref())?;
                    element_types.push(ty);
                }
                let element_type = if element_types.is_empty() {
                    TypeInfo::Any
                } else {
                    TypeInfo::from_union(element_types)
                };
                Some(TypeInfo::Array(Box::new(element_type)))
            }
            _ => None,
        }
    }

    fn infer_literal_value_type(expr: &Expr) -> Option<TypeInfo> {
        match expr {
            Expr::Lit(lit) => match lit {
                Lit::Str(_) => Some(TypeInfo::String),
                Lit::Num(_) => Some(TypeInfo::Number),
                Lit::Bool(_) => Some(TypeInfo::Boolean),
                Lit::Null(_) => Some(TypeInfo::Null),
                Lit::BigInt(_) => Some(TypeInfo::Number),
                _ => Some(TypeInfo::Any),
            },
            Expr::Array(array) => {
                let mut element_types: Vec<TypeInfo> = Vec::new();
                for elem in array.elems.iter().flatten() {
                    if elem.spread.is_some() {
                        return Some(TypeInfo::Any);
                    }
                    let ty = TagScanner::infer_literal_value_type(elem.expr.as_ref())?;
                    element_types.push(ty);
                }
                let element_type = if element_types.is_empty() {
                    TypeInfo::Any
                } else {
                    TypeInfo::from_union(element_types)
                };
                Some(TypeInfo::Array(Box::new(element_type)))
            }
            _ => None,
        }
    }

    fn parse_identifier_chain(expr: &str) -> Option<String> {
        let trimmed = expr.trim();
        if trimmed.is_empty() {
            return None;
        }

        let mut parts = Vec::new();
        for segment in trimmed.split('.') {
            let candidate = segment.trim();
            if candidate.is_empty() || !Self::is_valid_identifier(candidate) {
                return None;
            }
            parts.push(candidate);
        }

        if parts.is_empty() {
            None
        } else {
            Some(parts.join("."))
        }
    }

    fn extract_pattern_bindings(
        pattern: &str,
        base_type: Option<&TypeInfo>,
        fallback: &TypeInfo,
    ) -> Vec<StateVariable> {
        let mut results = Vec::new();
        let trimmed = pattern.trim();
        if trimmed.is_empty() {
            return results;
        }

        if trimmed.starts_with('{') && trimmed.ends_with('}') {
            let inner = &trimmed[1..trimmed.len().saturating_sub(1)];
            for segment in inner.split(',') {
                let segment = segment.trim();
                if segment.is_empty() || segment.starts_with("...") {
                    continue;
                }

                let mut parts = segment.splitn(2, ':');
                let source = parts.next().unwrap_or("").trim();
                if source.is_empty() {
                    continue;
                }
                let target = parts
                    .next()
                    .map(|s| s.trim())
                    .filter(|s| !s.is_empty())
                    .unwrap_or(source);

                if !Self::is_valid_identifier(target) {
                    continue;
                }

                let property_name = Self::normalize_property_key(source);
                if property_name.is_empty() {
                    continue;
                }

                let property_type = base_type
                    .map(|base| resolve_member_type(base, &property_name))
                    .filter(|ty| !matches!(ty, TypeInfo::Any))
                    .unwrap_or_else(|| (*fallback).clone());

                results.push(StateVariable {
                    name: target.to_string(),
                    ty: property_type,
                });
            }
        } else if trimmed.starts_with('[') && trimmed.ends_with(']') {
            let inner = &trimmed[1..trimmed.len().saturating_sub(1)];
            let segments = Self::split_array_elements(inner);
            for (idx, segment) in segments.into_iter().enumerate() {
                let mut segment = segment.trim();
                if segment.is_empty() || segment.starts_with("...") {
                    continue;
                }
                if let Some(eq_idx) = segment.find('=') {
                    segment = segment[..eq_idx].trim();
                }
                if segment.is_empty() {
                    continue;
                }
                let target = segment
                    .split(':')
                    .last()
                    .map(|s| s.trim())
                    .unwrap_or("");
                if target.is_empty() || !Self::is_valid_identifier(target) {
                    continue;
                }

                let ty = base_type
                    .and_then(|base| Self::array_pattern_element_type(base, idx))
                    .unwrap_or_else(|| (*fallback).clone());
                results.push(StateVariable {
                    name: target.to_string(),
                    ty,
                });
            }
        } else if Self::is_valid_identifier(trimmed) {
            let ty = base_type.cloned().unwrap_or_else(|| (*fallback).clone());
            results.push(StateVariable {
                name: trimmed.to_string(),
                ty,
            });
        }

        results
    }

    fn is_array_pattern(text: &str) -> bool {
        let trimmed = text.trim();
        trimmed.starts_with('[') && trimmed.ends_with(']')
    }

    fn split_array_elements(inner: &str) -> Vec<String> {
        let mut segments = Vec::new();
        let mut depth = 0i32;
        let mut start = 0;
        for (idx, ch) in inner.char_indices() {
            match ch {
                '[' | '{' | '(' => depth += 1,
                ']' | '}' | ')' => {
                    if depth > 0 {
                        depth -= 1;
                    }
                }
                ',' if depth == 0 => {
                    segments.push(inner[start..idx].to_string());
                    start = idx + 1;
                    continue;
                }
                _ => {}
            }
        }
        if start < inner.len() {
            segments.push(inner[start..].to_string());
        }
        segments
    }

    fn array_pattern_element_type(base: &TypeInfo, index: usize) -> Option<TypeInfo> {
        match base {
            TypeInfo::Tuple(items) => items.get(index).cloned(),
            TypeInfo::Array(inner) => {
                if index == 0 {
                    Some((**inner).clone())
                } else {
                    None
                }
            }
            TypeInfo::Set(inner) => {
                if index == 0 {
                    Some((**inner).clone())
                } else {
                    None
                }
            }
            TypeInfo::Union(options) => {
                let mut collected = Vec::new();
                for option in options {
                    if let Some(ty) = Self::array_pattern_element_type(option, index) {
                        collected.push(ty);
                    }
                }
                if collected.is_empty() {
                    None
                } else if collected.iter().all(|ty| ty == &collected[0]) {
                    Some(collected[0].clone())
                } else {
                    Some(TypeInfo::Any)
                }
            }
            _ => None,
        }
    }

    fn normalize_property_key(source: &str) -> String {
        let trimmed = source.trim();
        if trimmed.len() >= 2
            && ((trimmed.starts_with('"') && trimmed.ends_with('"'))
                || (trimmed.starts_with('\'') && trimmed.ends_with('\'')))
        {
            trimmed[1..trimmed.len() - 1].to_string()
        } else {
            trimmed.to_string()
        }
    }

    fn ensure_global_helpers(locals: &mut HashMap<String, TypeInfo>) {
        if !locals.contains_key("$") {
            locals.insert("$".to_string(), Self::dollar_helpers_type());
        }
    }

    fn dollar_helpers_type() -> TypeInfo {
        let mut props = HashMap::new();
        props.insert(
            "get".to_string(),
            TypeInfo::Named(
                "get(url: string | URL, fetchOps?: RequestInit, defaultOps?: Omit<import(\"./fetch.ts\").FetchDOMOptions, \"to\" | \"method\">): import(\"./fetch.ts\").FetchedContent"
                    .to_string(),
            ),
        );
        props.insert(
            "post".to_string(),
            TypeInfo::Named(
                "post(url: string | URL, fetchOps?: RequestInit, defaultOps?: Omit<import(\"./fetch.ts\").FetchDOMOptions, \"to\" | \"method\">): import(\"./fetch.ts\").FetchedContent"
                    .to_string(),
            ),
        );
        props.insert(
            "fetch".to_string(),
            TypeInfo::Named(
                "fetch(url: string | URL, ops: import(\"./fetch.ts\").FetchDOMOptions, fetchOps?: RequestInit): Promise<void>"
                    .to_string(),
            ),
        );
        TypeInfo::Object(props)
    }

    fn split_each_bindings(bindings: &str) -> (&str, Option<&str>) {
        let mut depth = 0i32;
        for (idx, ch) in bindings.char_indices() {
            match ch {
                '{' | '[' | '(' => depth += 1,
                '}' | ']' | ')' => {
                    if depth > 0 {
                        depth -= 1;
                    }
                }
                ',' if depth == 0 => {
                    let item = bindings[..idx].trim();
                    let key = bindings[idx + 1..].trim();
                    let key_opt = if key.is_empty() { None } else { Some(key) };
                    return (item, key_opt);
                }
                _ => {}
            }
        }

        (bindings.trim(), None)
    }

    fn infer_each_index_type(collection_type: &TypeInfo) -> TypeInfo {
        match collection_type {
            TypeInfo::Map(key, _) => (*key.clone()).clone(),
            TypeInfo::Object(_) => TypeInfo::String,
            TypeInfo::Union(options) => {
                let mut collected = Vec::new();
                for option in options {
                    collected.push(Self::infer_each_index_type(option));
                }
                TypeInfo::from_union(collected)
            }
            _ => TypeInfo::Number,
        }
    }

    fn infer_each_value_type(collection_type: &TypeInfo) -> TypeInfo {
        match collection_type {
            TypeInfo::Array(inner) | TypeInfo::Set(inner) => (**inner).clone(),
            TypeInfo::Map(_, value) => (**value).clone(),
            TypeInfo::Object(props) => {
                if props.is_empty() {
                    TypeInfo::Any
                } else {
                    TypeInfo::from_union(props.values().cloned().collect())
                }
            }
            TypeInfo::Tuple(items) => items.first().cloned().unwrap_or(TypeInfo::Any),
            TypeInfo::Union(options) => {
                let mut collected = Vec::new();
                for option in options {
                    collected.push(Self::infer_each_value_type(option));
                }
                TypeInfo::from_union(collected)
            }
            TypeInfo::Promise(inner) => Self::infer_each_value_type(inner),
            _ => TypeInfo::Any,
        }
    }

    fn is_valid_identifier(name: &str) -> bool {
        let mut chars = name.chars();
        let Some(first) = chars.next() else {
            return false;
        };
        if !Self::is_valid_identifier_start(first) {
            return false;
        }
        chars.all(Self::is_valid_identifier_part)
    }

    fn is_valid_identifier_start(ch: char) -> bool {
        ch == '_' || ch == '$' || ch.is_ascii_alphabetic()
    }

    fn is_valid_identifier_part(ch: char) -> bool {
        Self::is_valid_identifier_start(ch) || ch.is_ascii_digit()
    }
}
