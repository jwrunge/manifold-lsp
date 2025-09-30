use std::collections::HashMap;

use crate::manifold_lang::{DirectiveType, ManifoldElement, Spanned};

pub enum ImCompleteCompletionItem {
    ManifoldDirective(String, String), // directive name, description
    ManifoldAttribute(String),
    ManifoldVariable(String),
    ManifoldFunction(String, Vec<String>),
    HtmlTag(String),
    HtmlAttribute(String),
}

/// Provide completions for Manifold features in HTML
pub fn completion(
    ast: &[Spanned<ManifoldElement>],
    offset: usize,
) -> HashMap<String, ImCompleteCompletionItem> {
    let mut completions = HashMap::new();

    // Add common Manifold directives
    add_manifold_directives(&mut completions);

    // Add common HTML elements and attributes
    add_html_completions(&mut completions);

    // Add Manifold-specific attributes
    add_manifold_attributes(&mut completions);

    // Search for variables in scope
    for (element, _span) in ast.iter() {
        collect_variables_from_element(element, &mut completions, offset);
    }

    completions
}

fn add_manifold_directives(completions: &mut HashMap<String, ImCompleteCompletionItem>) {
    let directives = vec![
        (
            ":if",
            "Conditional rendering - show element if expression is truthy",
        ),
        (":elseif", "Alternative condition for :if directive"),
        (":else", "Fallback for :if directive"),
        (
            ":each",
            "Loop directive - repeat element for each item in array",
        ),
        (":await", "Async directive - handle promise resolution"),
        (":then", "Success handler for :await directive"),
        (":catch", "Error handler for :await directive"),
        (":onclick", "Click event handler"),
        (":oninput", "Input event handler"),
        (":onchange", "Change event handler"),
        (":onsubmit", "Submit event handler"),
        (":value", "Bind value to element"),
        (":checked", "Bind checked state to checkbox/radio"),
        (":sync:value", "Two-way binding for input value"),
        (":sync:checked", "Two-way binding for checkbox state"),
        (":class:", "Conditional CSS class binding"),
        (":style:", "Conditional CSS style binding"),
        (":transition", "Animation transition class"),
    ];

    for (directive, description) in directives {
        completions.insert(
            directive.to_string(),
            ImCompleteCompletionItem::ManifoldDirective(
                directive.to_string(),
                description.to_string(),
            ),
        );
    }
}

fn add_html_completions(completions: &mut HashMap<String, ImCompleteCompletionItem>) {
    let html_tags = vec![
        "div", "span", "p", "h1", "h2", "h3", "h4", "h5", "h6", "ul", "ol", "li", "a", "img",
        "input", "button", "form", "section", "article", "header", "footer", "nav", "main",
        "table", "tr", "td", "th", "thead", "tbody", "select", "option",
    ];

    for tag in html_tags {
        completions.insert(
            tag.to_string(),
            ImCompleteCompletionItem::HtmlTag(tag.to_string()),
        );
    }

    let html_attributes = vec![
        "id",
        "class",
        "style",
        "title",
        "lang",
        "dir",
        "src",
        "alt",
        "href",
        "target",
        "type",
        "name",
        "value",
        "placeholder",
        "required",
        "disabled",
        "readonly",
    ];

    for attr in html_attributes {
        completions.insert(
            attr.to_string(),
            ImCompleteCompletionItem::HtmlAttribute(attr.to_string()),
        );
    }
}

fn add_manifold_attributes(completions: &mut HashMap<String, ImCompleteCompletionItem>) {
    let manifold_attrs = vec![
        (
            "data-mf-register",
            "Register element for Manifold reactivity",
        ),
        (
            "data-mf-ignore",
            "Ignore element and its children from Manifold processing",
        ),
    ];

    for (attr, _description) in manifold_attrs {
        completions.insert(
            attr.to_string(),
            ImCompleteCompletionItem::ManifoldAttribute(attr.to_string()),
        );
    }
}

fn collect_variables_from_element(
    element: &ManifoldElement,
    completions: &mut HashMap<String, ImCompleteCompletionItem>,
    _offset: usize,
) {
    match element {
        ManifoldElement::HtmlElement { children, .. } => {
            for child in children {
                collect_variables_from_element(child, completions, _offset);
            }
        }
        ManifoldElement::TextContent { interpolations, .. } => {
            for interpolation in interpolations {
                // In a real implementation, we'd parse the expression to find variables
                let expr = interpolation.expression.trim();
                if is_simple_identifier(expr) {
                    completions.insert(
                        expr.to_string(),
                        ImCompleteCompletionItem::ManifoldVariable(expr.to_string()),
                    );
                }
            }
        }
        _ => {}
    }
}

fn is_simple_identifier(s: &str) -> bool {
    !s.is_empty()
        && s.chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '$')
}

/// Get completion suggestions based on the current context and cursor position
pub fn get_context_sensitive_completions(
    ast: &[Spanned<ManifoldElement>],
    offset: usize,
    line_text: &str,
    character: usize,
) -> HashMap<String, ImCompleteCompletionItem> {
    let mut completions = HashMap::new();

    // Check if we're inside an attribute name context
    if is_in_attribute_context(line_text, character) {
        add_manifold_directives(&mut completions);
        add_manifold_attributes(&mut completions);
        return completions;
    }

    // Check if we're inside an interpolation ${...}
    if is_in_interpolation_context(line_text, character) {
        // Add variables and functions available in current scope
        add_manifold_variables(&mut completions);
        return completions;
    }

    // Default to all completions
    completion(ast, offset)
}

fn is_in_attribute_context(line_text: &str, character: usize) -> bool {
    if character == 0 {
        return false;
    }

    // Look for pattern like "<tag " or "<tag attr" where cursor is in attribute position
    let up_to_cursor = &line_text[..character.min(line_text.len())];

    // Simple heuristic: if there's a < before us and no > after the <, we're likely in a tag
    if let Some(last_lt) = up_to_cursor.rfind('<') {
        if let Some(last_gt) = up_to_cursor.rfind('>') {
            last_lt > last_gt
        } else {
            true
        }
    } else {
        false
    }
}

fn is_in_interpolation_context(line_text: &str, character: usize) -> bool {
    if character < 2 {
        return false;
    }

    let up_to_cursor = &line_text[..character.min(line_text.len())];

    // Look for ${...} pattern where cursor is inside
    if let Some(last_dollar_brace) = up_to_cursor.rfind("${") {
        // Check if there's a closing brace after the ${ but before cursor
        let after_dollar_brace = &up_to_cursor[last_dollar_brace + 2..];
        !after_dollar_brace.contains('}')
    } else {
        false
    }
}

fn add_manifold_variables(completions: &mut HashMap<String, ImCompleteCompletionItem>) {
    // Common Manifold variables/functions
    let variables = vec![
        ("$", "Manifold utility object"),
        ("$.get", "HTTP GET utility function"),
        ("$.post", "HTTP POST utility function"),
        ("state", "Current component state"),
        ("count", "Example counter variable"),
        ("user", "Example user object"),
        ("items", "Example items array"),
    ];

    for (var, _description) in variables {
        completions.insert(
            var.to_string(),
            ImCompleteCompletionItem::ManifoldVariable(var.to_string()),
        );
    }
}
