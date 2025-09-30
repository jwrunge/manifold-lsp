use regex::Regex;
use std::collections::HashMap;
use tower_lsp::lsp_types::{Position, Range};

#[derive(Debug, Clone)]
pub struct ManifoldExtract {
    pub content: String,
    pub range: Range,
    pub extract_type: ExtractType,
}

#[derive(Debug, Clone)]
pub enum ExtractType {
    Interpolation,
    Directive { attribute_name: String },
    DataAttribute { attribute_name: String },
}

/// Extract only Manifold-specific content from HTML, leaving everything else for the HTML language server
pub fn extract_manifold_content(text: &str) -> Vec<ManifoldExtract> {
    let mut extracts = Vec::new();
    let lines: Vec<&str> = text.lines().collect();

    // Regex patterns for Manifold-specific content
    let interpolation_regex = Regex::new(r"\$\{([^}]+)\}").unwrap();
    let directive_regex = Regex::new(r#":(\w+(?::\w+)?)=["']([^"']+)["']"#).unwrap();
    let data_mf_regex = Regex::new(r#"data-mf-(\w+)(?:=["']([^"']*?)["'])?"#).unwrap();

    for (line_idx, line) in lines.iter().enumerate() {
        let line_start_offset = lines[..line_idx].iter().map(|l| l.len() + 1).sum::<usize>();

        // Extract ${} interpolations
        for cap in interpolation_regex.captures_iter(line) {
            if let Some(matched) = cap.get(0) {
                let start_col = matched.start();
                let end_col = matched.end();

                extracts.push(ManifoldExtract {
                    content: cap.get(1).unwrap().as_str().to_string(),
                    range: Range::new(
                        Position::new(line_idx as u32, start_col as u32),
                        Position::new(line_idx as u32, end_col as u32),
                    ),
                    extract_type: ExtractType::Interpolation,
                });
            }
        }

        // Extract :directive attributes
        for cap in directive_regex.captures_iter(line) {
            if let Some(matched) = cap.get(0) {
                let start_col = matched.start();
                let end_col = matched.end();
                let directive_name = cap.get(1).unwrap().as_str().to_string();
                let directive_value = cap.get(2).unwrap().as_str().to_string();

                extracts.push(ManifoldExtract {
                    content: directive_value,
                    range: Range::new(
                        Position::new(line_idx as u32, start_col as u32),
                        Position::new(line_idx as u32, end_col as u32),
                    ),
                    extract_type: ExtractType::Directive {
                        attribute_name: directive_name,
                    },
                });
            }
        }

        // Extract data-mf- attributes
        for cap in data_mf_regex.captures_iter(line) {
            if let Some(matched) = cap.get(0) {
                let start_col = matched.start();
                let end_col = matched.end();
                let attr_name = format!("data-mf-{}", cap.get(1).unwrap().as_str());
                let attr_value = cap.get(2).map(|m| m.as_str()).unwrap_or("").to_string();

                extracts.push(ManifoldExtract {
                    content: attr_value,
                    range: Range::new(
                        Position::new(line_idx as u32, start_col as u32),
                        Position::new(line_idx as u32, end_col as u32),
                    ),
                    extract_type: ExtractType::DataAttribute {
                        attribute_name: attr_name,
                    },
                });
            }
        }
    }

    extracts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_interpolations() {
        let html = r#"<p>Hello ${name}! Count: ${count}</p>"#;
        let extracts = extract_manifold_content(html);

        assert_eq!(extracts.len(), 2);
        assert!(matches!(
            extracts[0].extract_type,
            ExtractType::Interpolation
        ));
        assert_eq!(extracts[0].content, "name");
        assert_eq!(extracts[1].content, "count");
    }

    #[test]
    fn test_extract_directives() {
        let html = r#"<button :onclick="increment()" :class="buttonClass">Click</button>"#;
        let extracts = extract_manifold_content(html);

        assert_eq!(extracts.len(), 2);
        assert!(matches!(
            extracts[0].extract_type,
            ExtractType::Directive { .. }
        ));
    }

    #[test]
    fn test_extract_data_mf() {
        let html = r#"<div data-mf-register="myContext" data-mf-ignore></div>"#;
        let extracts = extract_manifold_content(html);

        assert_eq!(extracts.len(), 2);
        assert!(matches!(
            extracts[0].extract_type,
            ExtractType::DataAttribute { .. }
        ));
    }
}
