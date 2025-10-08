use tower_lsp::lsp_types::{Position, Range};

use super::attribute::{ManifoldAttribute, ParsedAttribute};
use super::lineindex::LineIndex;

#[derive(Debug, Clone)]
pub struct ManifoldDocument {
    pub attributes: Vec<ManifoldAttribute>,
    pub line_index: LineIndex,
}

impl ManifoldDocument {
    pub fn parse(text: &str) -> Self {
        let line_index = LineIndex::new(text);
        let attributes = Self::extract_manifold_attributes(text, &line_index);
        Self {
            attributes,
            line_index,
        }
    }

    pub fn extract_manifold_attributes(
        text: &str,
        line_index: &LineIndex,
    ) -> Vec<ManifoldAttribute> {
        let bytes = text.as_bytes();
        let len = bytes.len();
        let mut idx = 0;
        let mut results = Vec::new();

        while idx < len {
            let Some(rel) = text[idx..].find('<') else {
                break;
            };
            let tag_start = idx + rel;
            idx = tag_start + 1;

            if idx >= len {
                break;
            }

            let next_char = bytes[idx];
            if matches!(next_char, b'/' | b'!' | b'?') {
                continue;
            }

            let mut cursor = idx;
            let mut in_quote: Option<u8> = None;
            while cursor < len {
                let ch = bytes[cursor];
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
                break;
            }

            let tag_end = cursor;
            idx = cursor + 1;

            let mut attr_cursor = tag_start + 1;
            while attr_cursor < tag_end {
                let ch = bytes[attr_cursor];
                if ch.is_ascii_whitespace() {
                    attr_cursor += 1;
                    break;
                }
                if matches!(ch, b'/' | b'>') {
                    break;
                }
                attr_cursor += 1;
            }

            let mut has_data_mf_register = false;
            let mut attributes: Vec<ParsedAttribute> = Vec::new();

            while attr_cursor < tag_end {
                while attr_cursor < tag_end && bytes[attr_cursor].is_ascii_whitespace() {
                    attr_cursor += 1;
                }
                if attr_cursor >= tag_end {
                    break;
                }
                if bytes[attr_cursor] == b'/' {
                    attr_cursor += 1;
                    continue;
                }

                let attr_name_start = attr_cursor;
                while attr_cursor < tag_end
                    && !bytes[attr_cursor].is_ascii_whitespace()
                    && !matches!(bytes[attr_cursor], b'=' | b'/')
                {
                    attr_cursor += 1;
                }
                let attr_name_end = attr_cursor;
                if attr_name_start >= attr_name_end {
                    break;
                }

                while attr_cursor < tag_end && bytes[attr_cursor].is_ascii_whitespace() {
                    attr_cursor += 1;
                }

                let (attr_end, value_range) = if attr_cursor < tag_end && bytes[attr_cursor] == b'='
                {
                    attr_cursor += 1;
                    while attr_cursor < tag_end && bytes[attr_cursor].is_ascii_whitespace() {
                        attr_cursor += 1;
                    }

                    if attr_cursor < tag_end {
                        if matches!(bytes[attr_cursor], b'"' | b'\'') {
                            let quote = bytes[attr_cursor];
                            attr_cursor += 1;
                            let value_start = attr_cursor;
                            while attr_cursor < tag_end && bytes[attr_cursor] != quote {
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
                                && !bytes[attr_cursor].is_ascii_whitespace()
                                && bytes[attr_cursor] != b'/'
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

                let name = text[attr_name_start..attr_name_end].to_string();
                let name_lower = name.to_ascii_lowercase();
                if name_lower == "data-mf-register" {
                    has_data_mf_register = true;
                } else {
                    attributes.push(ParsedAttribute {
                        name,
                        name_lower,
                        span_start: attr_name_start,
                        span_end: attr_end,
                        value_range,
                    });
                }
            }

            if has_data_mf_register {
                for attr in attributes.into_iter() {
                    if attr.name.starts_with(':') || attr.name_lower.starts_with("data-mf") {
                        let (start, end) = match attr.value_range {
                            Some((value_start, value_end)) if value_end > value_start => {
                                (value_start, value_end)
                            }
                            _ => (attr.span_start, attr.span_end),
                        };
                        let range =
                            Range::new(line_index.position_at(start), line_index.position_at(end));
                        results.push(ManifoldAttribute {
                            name: attr.name,
                            range,
                            start_offset: start,
                            end_offset: end,
                        });
                    }
                }
            }
        }

        results
    }

    pub fn manifold_attribute_at(&self, position: &Position) -> Option<&ManifoldAttribute> {
        let offset = self.line_index.offset_at(position)?;
        self.attributes
            .iter()
            .find(|attr| offset >= attr.start_offset && offset < attr.end_offset)
    }
}
