//! Semantic styles for the editor. Widgets receive the active theme at render time.

use ratatui::style::Style;

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
        // Reset uses the terminal's configured foreground/background, including transparency
        let base = Style::reset();
        Self {
            base,
            gutter: base,
            current_line: base.reversed(),
            selection: base.reversed().bold(),
            cursor_normal: base,
            cursor_insert: base.reversed().bold().underlined(),
            status: base.reversed(),
            panel: base,
            selected_item: base.reversed().bold(),
            prompt: base.bold(),
            directory: base,
            emphasis: Style::new().bold(),
        }
    }
}
