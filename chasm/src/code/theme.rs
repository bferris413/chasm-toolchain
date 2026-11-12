//! Semantic styles for the editor. Widgets receive the active theme at render time.

use ratatui::style::{Color, Style};

#[derive(Clone, Debug)]
pub(super) struct Theme {
    pub base: Style,
    pub gutter: Style,
    pub current_line: Style,
    pub selection: Style,
    pub cursor_normal: Style,
    pub cursor_insert: Style,
    pub status: Style,
    pub panel: Style,
    pub selected_item: Style,
    pub prompt: Style,
    pub directory: Style,
    pub emphasis: Style,
}

impl Default for Theme {
    fn default() -> Self {
        // Explicit RGB colors keep contrast independent of the terminal palette.
        let background = Color::Rgb(0x18, 0x1a, 0x1b);
        let text = Color::Rgb(0xd4, 0xd4, 0xd4);
        let base = Style::reset().fg(text).bg(background);
        let selection = base.bg(Color::Rgb(0x36, 0x46, 0x50));
        let cursor = base.fg(background).bg(Color::Rgb(0xde, 0xde, 0xde));
        let panel = base.bg(Color::Rgb(0x25, 0x29, 0x2c));
        Self {
            base,
            gutter: base.fg(Color::Rgb(0x77, 0x7c, 0x80)),
            // Preserve gutter foreground while highlighting the whole row.
            current_line: Style::new().bg(Color::Rgb(0x22, 0x25, 0x27)),
            selection,
            cursor_normal: cursor,
            cursor_insert: cursor.underlined(),
            status: panel,
            panel,
            selected_item: selection,
            prompt: Style::new().bold(),
            directory: Style::new(),
            emphasis: Style::new().bold(),
        }
    }
}
