use crate::code::theme::Theme;
use crossterm::event::{Event, KeyCode, KeyEventKind, KeyModifiers};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    text::{Line, Span},
    widgets::{Paragraph, Widget},
};

use crate::code::{ChasmWidget, WidgetContext};

#[derive(Debug)]
pub struct Input {
    pub input: String,
    pub cursor_position: usize,
}

impl Input {
    pub fn new(input: String) -> Self {
        let cursor_position = input.chars().count();
        Self {
            input,
            cursor_position,
        }
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    fn char_count(&self) -> usize {
        self.input.chars().count()
    }

    fn byte_index_at_char(&self, char_idx: usize) -> usize {
        if char_idx == 0 {
            return 0;
        }

        self.input
            .char_indices()
            .nth(char_idx)
            .map(|(idx, _)| idx)
            .unwrap_or(self.input.len())
    }

    fn move_left(&mut self) {
        self.cursor_position = self.cursor_position.saturating_sub(1);
    }

    fn move_right(&mut self) {
        self.cursor_position = self
            .cursor_position
            .saturating_add(1)
            .min(self.char_count());
    }

    fn move_to_start(&mut self) {
        self.cursor_position = 0;
    }

    fn move_to_end(&mut self) {
        self.cursor_position = self.char_count();
    }

    fn insert_char(&mut self, c: char) {
        let byte_idx = self.byte_index_at_char(self.cursor_position);
        self.input.insert(byte_idx, c);
        self.cursor_position += 1;
    }

    fn backspace(&mut self) {
        if self.cursor_position == 0 {
            return;
        }

        let remove_char_idx = self.cursor_position - 1;
        let remove_byte_idx = self.byte_index_at_char(remove_char_idx);
        self.input.remove(remove_byte_idx);
        self.cursor_position = remove_char_idx;
    }

    fn delete(&mut self) {
        if self.cursor_position >= self.char_count() {
            return;
        }

        let remove_byte_idx = self.byte_index_at_char(self.cursor_position);
        self.input.remove(remove_byte_idx);
    }
}

impl ChasmWidget for Input {
    fn handle_event(&mut self, event: &Event, _ctx: &mut WidgetContext) {
        let Event::Key(key_event) = event else {
            return;
        };

        if key_event.kind != KeyEventKind::Press {
            return;
        }

        match key_event.code {
            KeyCode::Left => self.move_left(),
            KeyCode::Right => self.move_right(),
            KeyCode::Home => self.move_to_start(),
            KeyCode::End => self.move_to_end(),
            KeyCode::Backspace => self.backspace(),
            KeyCode::Delete => self.delete(),
            KeyCode::Char(c) => {
                if c == 'a' && key_event.modifiers == KeyModifiers::CONTROL {
                    self.move_to_start();
                } else if c == 'e' && key_event.modifiers == KeyModifiers::CONTROL {
                    self.move_to_end();
                } else if c == 'k' && key_event.modifiers == KeyModifiers::CONTROL {
                    self.move_to_start();
                    self.input.clear();
                } else {
                    self.insert_char(c);
                }
            }
            _ => {}
        }
    }

    fn render(&mut self, area: Rect, buf: &mut Buffer, theme: &Theme) {
        let width = area.width as usize;
        if width == 0 {
            return;
        }

        let mut start = 0usize;
        if self.cursor_position >= width {
            start = self.cursor_position + 1 - width;
        }

        let mut content = self
            .input
            .chars()
            .skip(start)
            .take(width)
            .collect::<String>();

        if content.len() < width {
            content.push_str(&" ".repeat(width - content.len()));
        }

        let cursor_col = self.cursor_position.saturating_sub(start).min(width - 1);

        let chars = content.chars().collect::<Vec<char>>();
        if let Some(ch) = chars.get(cursor_col).copied() {
            let mut spans = Vec::with_capacity(width + 2);
            let prefix = chars[..cursor_col].iter().collect::<String>();
            if !prefix.is_empty() {
                spans.push(Span::raw(prefix));
            }

            spans.push(Span::styled(
                ch.to_string(),
                theme.cursor_insert,
            ));

            let suffix = chars[cursor_col + 1..].iter().collect::<String>();
            if !suffix.is_empty() {
                spans.push(Span::raw(suffix));
            }

            Paragraph::new(Line::from(spans)).style(theme.base).render(area, buf);
        }
    }
}