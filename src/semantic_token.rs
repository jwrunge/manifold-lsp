use std::collections::HashMap;

use tower_lsp::lsp_types::SemanticTokenType;

use crate::manifold_lang::{ImCompleteSemanticToken, ManifoldElement, Spanned};

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::REGEXP,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::TYPE,
    SemanticTokenType::STRUCT,
    SemanticTokenType::CLASS,
    SemanticTokenType::INTERFACE,
    SemanticTokenType::ENUM,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::METHOD,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::COMMENT,
    SemanticTokenType::MACRO,
];

pub fn semantic_token_from_ast(
    _ast: &HashMap<String, ManifoldElement>,
) -> Vec<ImCompleteSemanticToken> {
    // Legacy function - semantic tokens are now generated in the parser
    Vec::new()
}
