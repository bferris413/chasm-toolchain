//! Status bar widget functionality.
//! 
//! Displays app title, event information, and othe metadata.

use std::fmt::Write;

use crossterm::event::{Event, KeyEventKind};
use ratatui::{
    buffer::Buffer, layout::Rect, style::{Color, Style}, text::{Line, Span}, widgets::{Block, Paragraph, Widget, WidgetRef}
};

use crate::code::{ChasmWidget, WidgetContext};

#[derive(Debug)]
pub (super) struct StatusBar {
    title: &'static str,
    event_text: String,
    event_max_len: usize,
    search_text: String,
    search_max_len: usize,
}
impl StatusBar {
    pub fn new() -> Self {
        let event_len = 15;
        let search_len = 40;
        let title = " Chasm Editor";

        Self {
            title,
            event_text: String::with_capacity(event_len),
            event_max_len: event_len,
            search_text: String::with_capacity(search_len),
            search_max_len: search_len,
         }
     }
     
    pub(crate) fn set_search_input(&mut self, input: String) {
        self.search_text.clear();
        write!(&mut self.search_text, "{input}").unwrap();
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

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let block = Block::default().style(
            Style::new().bg(Color::LightCyan).fg(Color::Black)
        );

        let event_text = &self.event_text[..(self.event_max_len.min(self.event_text.len()))];
        let search_text = &self.search_text[..(self.search_max_len.min(self.search_text.len()))];

        let line = Line::from(vec![
            Span::styled(self.title, Style::new().bold()),
            Span::raw(" | "),
            Span::raw(format!("{event_text:^event_len$}", event_len = self.event_max_len)),
            Span::raw(" | "),
            Span::raw(format!("{search_text:search_len$}", search_len = self.search_max_len)),
            Span::raw(" | "),
        ]);

        Paragraph::new(line)
            .block(block)
            .render(area, buf)
    }
}
impl WidgetRef for StatusBar {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        ChasmWidget::render(self, area, buf);
    }
}
impl Widget for &StatusBar {
    fn render(self, area: Rect, buf: &mut Buffer) {
        ChasmWidget::render(self, area, buf);
    }
}