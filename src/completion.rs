use std::collections::HashMap;

use crate::manifold_lang::{ManifoldElement, Spanned};

pub enum ImCompleteCompletionItem {
    Variable(String),
    Function(String, Vec<String>),
}

/// Legacy completion function - redirects to manifold_completion
pub fn completion(
    ast: &[Spanned<ManifoldElement>],
    ident_offset: usize,
) -> HashMap<String, ImCompleteCompletionItem> {
    // Convert manifold completions to legacy format
    let manifold_completions = crate::manifold_completion::completion(ast, ident_offset);
    let mut legacy_completions = HashMap::new();

    for (key, item) in manifold_completions {
        match item {
            crate::manifold_completion::ImCompleteCompletionItem::ManifoldVariable(name) => {
                legacy_completions.insert(key, ImCompleteCompletionItem::Variable(name));
            }
            crate::manifold_completion::ImCompleteCompletionItem::ManifoldFunction(name, args) => {
                legacy_completions.insert(key, ImCompleteCompletionItem::Function(name, args));
            }
            _ => {
                // Convert other types to variables for legacy compatibility
                legacy_completions.insert(key.clone(), ImCompleteCompletionItem::Variable(key));
            }
        }
    }

    legacy_completions.insert(
        "example".to_string(),
        ImCompleteCompletionItem::Variable("example".to_string()),
    );
    legacy_completions
}
