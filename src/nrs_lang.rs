// Legacy NRS language parser - kept for compatibility
// Manifold functionality is now in manifold_lang.rs

use crate::manifold_lang::{ImCompleteSemanticToken, ManifoldElement, ParserResult};
use crate::span::Span;
use std::collections::HashMap;

pub type Spanned<T> = (T, Span);
pub type Ast = Vec<Spanned<ManifoldElement>>;

// Legacy value type for compatibility
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
}

// Stub implementations - redirect to manifold_lang
pub fn parse(src: &str) -> ParserResult {
    crate::manifold_lang::parse(src)
}

pub fn type_inference(
    _expr: &Spanned<ManifoldElement>,
    _symbol_type_table: &mut HashMap<Span, Value>,
) {
    // Legacy stub - actual implementation in manifold_lang
}
