use crossterm::event::{Event, KeyCode, KeyEventKind};
use ratatui::{buffer::Buffer, layout::Rect, style::{Style, Stylize}, widgets::{List, ListState, StatefulWidget}};

use crate::code::{ChasmWidget, WidgetContext, editor::SelectWidgetAction};


#[derive(Debug)]
pub (super) struct BufferSelect {
    select_list_state: ListState,
    list: List<'static>,
    max_chars: usize,
}
impl BufferSelect {
    pub fn new(items: Vec<String>) -> Self {
        // we control this, but sanity check until BufferSelect is wired directly to Buffers
        assert!(!items.is_empty(), "BufferSelect created with empty list");
        let max_chars = items.iter().map(|s| s.len()).max().unwrap();

        let list = List::new(items)
            .highlight_style(Style::new().reversed())
            .highlight_symbol(">> ".bold())
            .repeat_highlight_symbol(true);

        let mut list_state = ListState::default();
        list_state.select(Some(0));

        Self {
            select_list_state: list_state,
            list,
            max_chars,
        }
    }

    pub fn next(&mut self) {
        let len = self.list.len();
        // we control this via new()
        let cur_selected = self.select_list_state.selected().unwrap();

        let next = (cur_selected + 1) % len;
        self.select_list_state.select(Some(next as usize));
    }

    pub fn prev(&mut self) {
        let len = self.list.len();
        // we control this via new()
        let cur_selected = self.select_list_state.selected().unwrap();

        let next = if cur_selected == 0 {
            len - 1
        } else {
            cur_selected - 1
        };

        self.select_list_state.select(Some(next as usize));
    }
    
    pub fn selected(&self) -> usize {
        // we control this via new()
        self.select_list_state.selected().unwrap()
    }

    pub fn max_width(&self) -> usize {
        self.max_chars + 5
    }

    pub fn handle_editor_event(&mut self, event: &Event, _ctx: &mut WidgetContext) -> SelectWidgetAction {
        let Event::Key(key_event) = event else {
            return SelectWidgetAction::NoOp;
        };

        if let KeyEventKind::Press = key_event.kind {
            match key_event.code {
                KeyCode::Tab => {
                    self.next();
                    SelectWidgetAction::NoOp
                }
                KeyCode::Char('j') | KeyCode::Down => {
                    self.next();
                    SelectWidgetAction::NoOp
                }
                KeyCode::Char('k') | KeyCode::Up => {
                    self.prev();
                    SelectWidgetAction::NoOp
                }
                KeyCode::Esc => {
                    SelectWidgetAction::Cancel
                }
                KeyCode::Enter => {
                    let selected = self.selected();
                    SelectWidgetAction::SetActive(selected)
                }
                _ => {
                    // nothing to do
                    SelectWidgetAction::NoOp
                }
            }
        } else {
            SelectWidgetAction::NoOp
        }
    }
}
impl ChasmWidget for BufferSelect {
    fn handle_event(&mut self, _event: &Event, _ctx: &mut WidgetContext) {
        // BufferSelect doesn't handle any top-level events itself
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let mut state = self.select_list_state;
        StatefulWidget::render(&self.list, area, buf, &mut state);
    }
}