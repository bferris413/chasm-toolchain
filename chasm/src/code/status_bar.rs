//! Status bar widget functionality.
//! 
//! Displays app title, event information, and othe metadata.

use crate::code::theme::Theme;

use std::fmt::Write;

use crossterm::event::{Event, KeyEventKind};
use ratatui::{
    buffer::Buffer, layout::Rect, text::{Line, Span}, widgets::{Block, Paragraph, Widget}
};

use crate::code::{ChasmWidget, WidgetContext};

#[derive(Debug)]
pub (super) struct StatusBar {
    title: &'static str,

    event_text: String,
    event_max_len: usize,

    search_or_numeric_text: String,
    search_or_numeric_max_len: usize,

    cmd_text: String,
    cmd_max_len: usize,
}
impl StatusBar {
    pub fn new() -> Self {
        let event_len = 15;
        let search_len = 40;
        let cmd_len = 20;
        let title = " Chasm Editor";

        Self {
            title,
            event_text: String::with_capacity(event_len),
            event_max_len: event_len,
            search_or_numeric_text: String::with_capacity(search_len),
            search_or_numeric_max_len: search_len,
            cmd_text: String::with_capacity(cmd_len),
            cmd_max_len: cmd_len,
         }
     }
     
    pub(crate) fn set_search_or_numeric_input(&mut self, input: String) {
        self.search_or_numeric_text.clear();
        write!(&mut self.search_or_numeric_text, "{input}").unwrap();
    }

    pub (crate) fn set_command_input(&mut self, input: String) {
        self.cmd_text.clear();
        write!(&mut self.cmd_text, "{input}").unwrap();
    }
}
impl ChasmWidget for StatusBar {
    fn handle_event(&mut self, event: &Event, _ctx: &mut WidgetContext) {
        match event {
            Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.event_text.clear();
                let modifier = if key_event.modifiers.is_empty() {
                    String::new()
                } else {
                    format!("{}+", key_event.modifiers)
                };
                write!(&mut self.event_text, "{modifier}{}", key_event.code).unwrap();
            }
            other => {
                self.event_text.clear();
                write!(&mut self.event_text, "{:?}", other).unwrap();
            }
        };
    }

    fn render(&mut self, area: Rect, buf: &mut Buffer, theme: &Theme) {
        let block = Block::default().style(theme.status);

        let event_text = &self.event_text[..(self.event_max_len.min(self.event_text.len()))];
        let search_text = &self.search_or_numeric_text[..(self.search_or_numeric_max_len.min(self.search_or_numeric_text.len()))];
        let cmd_text = &self.cmd_text[..(self.cmd_max_len.min(self.cmd_text.len()))];

        let line = Line::from(vec![
            Span::styled(self.title, theme.emphasis),
            Span::raw(" "),
            Span::raw(format!("{event_text:^event_len$}", event_len = self.event_max_len)),
            Span::raw(" "),
            Span::styled(format!("{search_text:search_len$}", search_len = self.search_or_numeric_max_len), theme.emphasis),
            Span::raw(" "),
            Span::styled(format!("{cmd_text:cmd_len$}", cmd_len = self.cmd_max_len), theme.emphasis),
            Span::raw(" "),
        ]);

        Paragraph::new(line)
            .block(block)
            .render(area, buf)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::style::{Color, Modifier};

    #[test]
    fn emphasized_title_keeps_status_bar_reverse_video() {
        let area = Rect::new(0, 0, 100, 1);
        let mut buf = Buffer::empty(area);
        StatusBar::new().render(area, &mut buf, &Theme::default());

        assert_eq!(buf[(1, 0)].symbol(), "C");
        assert_eq!(buf[(1, 0)].modifier, Modifier::REVERSED | Modifier::BOLD);
        assert_eq!(buf[(99, 0)].modifier, Modifier::REVERSED);
        assert!(buf.content.iter().all(|cell| cell.fg == Color::Reset && cell.bg == Color::Reset));
    }
}
