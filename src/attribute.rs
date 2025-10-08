use tower_lsp::lsp_types::Range;

#[derive(Debug, Clone)]
pub struct ManifoldAttribute {
    pub name: String,
    pub range: Range,
    pub start_offset: usize,
    pub end_offset: usize,
}

#[derive(Debug)]
pub struct ParsedAttribute {
    pub name: String,
    pub name_lower: String,
    pub span_start: usize,
    pub span_end: usize,
    pub value_range: Option<(usize, usize)>,
}
