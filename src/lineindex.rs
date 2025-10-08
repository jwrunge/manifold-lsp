use tower_lsp::lsp_types::Position;

#[derive(Debug, Clone)]
pub struct LineIndex {
    line_starts: Vec<usize>,
    text_len: usize,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let mut line_starts = vec![0];
        for (idx, ch) in text.char_indices() {
            if ch == '\n' {
                let next = idx + ch.len_utf8();
                line_starts.push(next);
            }
        }
        Self {
            line_starts,
            text_len: text.len(),
        }
    }

    pub fn position_at(&self, offset: usize) -> Position {
        let line = match self.line_starts.binary_search(&offset) {
            Ok(index) => index,
            Err(index) => index.saturating_sub(1),
        };
        let line_start = self.line_starts.get(line).copied().unwrap_or(0);
        Position::new(line as u32, (offset.saturating_sub(line_start)) as u32)
    }

    pub fn offset_at(&self, position: &Position) -> Option<usize> {
        let line = position.line as usize;
        let character = position.character as usize;
        let line_start = *self.line_starts.get(line)?;
        let line_end = self
            .line_starts
            .get(line + 1)
            .copied()
            .unwrap_or(self.text_len);
        let offset = line_start + character;
        if offset > line_end {
            None
        } else {
            Some(offset)
        }
    }
}
