use tower_lsp::lsp_types::{Position, Range};

use super::attribute::{ManifoldAttribute, ManifoldAttributeKind, ParsedAttribute};
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
        TagScanner::new(text, line_index).collect_attributes()
    }

    pub fn manifold_attribute_at(&self, position: &Position) -> Option<&ManifoldAttribute> {
        let offset = self.line_index.offset_at(position)?;
        self.attributes
            .iter()
            .find(|attr| offset >= attr.start_offset && offset < attr.end_offset)
    }
}

struct TagScanner<'a> {
    text: &'a str,
    bytes: &'a [u8],
    line_index: &'a LineIndex,
    idx: usize,
    stack: Vec<AncestorState>,
    results: Vec<ManifoldAttribute>,
}

#[derive(Debug, Clone, Copy)]
struct AncestorState {
    registered: bool,
    ignore_active: bool,
}

impl<'a> TagScanner<'a> {
    fn new(text: &'a str, line_index: &'a LineIndex) -> Self {
        Self {
            text,
            bytes: text.as_bytes(),
            line_index,
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

        let mut new_state = parent_state;
        if parent_state.ignore_active {
            new_state.registered = false;
        }

        if saw_ignore {
            new_state.registered = false;
            new_state.ignore_active = true;
        } else if saw_register {
            new_state.registered = true;
            new_state.ignore_active = false;
        }

        if new_state.registered && !new_state.ignore_active {
            for attr in parsed_attributes.into_iter() {
                if attr.name.starts_with(':') || attr.name_lower.starts_with("data-mf") {
                    let (start, end) = match attr.value_range {
                        Some((value_start, value_end)) if value_end > value_start => {
                            (value_start, value_end)
                        }
                        _ => (attr.span_start, attr.span_end),
                    };
                    let range = Range::new(
                        self.line_index.position_at(start),
                        self.line_index.position_at(end),
                    );
                    self.results.push(ManifoldAttribute {
                        name: attr.name,
                        range,
                        start_offset: start,
                        end_offset: end,
                        kind: ManifoldAttributeKind::Attribute,
                    });
                }
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
        self.stack.last().copied().unwrap_or(AncestorState {
            registered: false,
            ignore_active: false,
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
            self.results.push(ManifoldAttribute {
                name,
                range,
                start_offset: expr_start,
                end_offset: expr_end,
                kind: ManifoldAttributeKind::TextExpression,
            });
            search_idx = expr_end;
        }
    }
}
