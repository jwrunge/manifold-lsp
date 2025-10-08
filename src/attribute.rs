use tower_lsp::lsp_types::Range;

use crate::state::{LoopBinding, StateVariable};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ManifoldAttributeKind {
    Attribute,
    TextExpression,
}

#[derive(Debug, Clone)]
pub struct ManifoldAttribute {
    pub name: String,
    pub range: Range,
    pub start_offset: usize,
    pub end_offset: usize,
    pub kind: ManifoldAttributeKind,
    pub expression: Option<String>,
    pub expression_span: Option<(usize, usize)>,
    pub state_name: Option<String>,
    pub locals: Vec<StateVariable>,
    pub loop_binding: Option<LoopBinding>,
}

#[derive(Debug)]
pub struct ParsedAttribute {
    pub name: String,
    pub name_lower: String,
    pub span_start: usize,
    pub span_end: usize,
    pub value_range: Option<(usize, usize)>,
}
