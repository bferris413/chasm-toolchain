mod buffer;
mod command;
mod numeric;
mod ops;
mod search;
mod undo_redo;

use std::{fmt, vec};
use std::path::PathBuf;
use std::ops::Range;

use crossterm::event::{Event, KeyCode, KeyEventKind, KeyModifiers};
use ratatui::buffer::Buffer;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Style, Stylize};
use ratatui::text::Line;
use ratatui::widgets::{Block, Borders, Clear, List, ListState, Paragraph, StatefulWidget, Widget};

use crate::code::editor::buffer::{BufferContext, BufferState};
use crate::code::editor::command::CommandBuffer;
use crate::code::editor::numeric::NumericBuffer;
use crate::code::{ChasmWidget, WidgetContext};
use crate::project::ModulePath;
use search::SearchBuffer;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum BufferMode {
    Normal,
    Insert,
}

#[derive(Debug)]
pub enum ModuleOrFile {
    Module(ModulePath),
    File(PathBuf),
}
impl ModuleOrFile {
    pub fn path(&self) -> &PathBuf {
        match self {
            ModuleOrFile::Module(module) => &module.path,
            ModuleOrFile::File(path) => path,
        }
    }
}

#[derive(Debug)]
struct Buffers {
    buf_states: Vec<BufferState>,
    mru_index: Vec<usize>,
}
impl Buffers {
    fn new(mut buf_states: Vec<BufferState>) -> Self {
        if buf_states.is_empty() {
            buf_states.push(BufferState::new(ModuleOrFile::File(PathBuf::new())));
        }

        let mru_index = (0..buf_states.len()).collect();
        Self { buf_states, mru_index }
    }

    fn active(&self) -> &BufferState {
        let active_idx = self.mru_index[0];
        &self.buf_states[active_idx]
    }

    fn active_mut(&mut self) -> &mut BufferState {
        let active_idx = self.mru_index[0];
        &mut self.buf_states[active_idx]
    }

    fn push(&mut self, buf_state: BufferState) {
        self.buf_states.push(buf_state);
        self.mru_index.insert(0, self.buf_states.len() - 1);
    }

    fn set_active(&mut self, index_of_id: usize) {
        assert!(index_of_id < self.buf_states.len(), "active index ({index_of_id}) >= buffer list length ({})", self.buf_states.len());
        let buf_id = self.mru_index.remove(index_of_id);
        self.mru_index.insert(0, buf_id);
    }

    fn remove_active(&mut self) {
        if self.buf_states.len() == 1 {
            self.buf_states.pop().expect("We always have at least one buffer");
            self.buf_states.push(BufferState::new(ModuleOrFile::File(PathBuf::new())));

            assert!(self.mru_index.len() == 1, "Invariant: we always have at least one index");
            assert_eq!(self.mru_index[0], 0, "MRU index should point to the only buffer we have");
        } else {
            assert!(! self.buf_states.is_empty(), "Invariant: we always have at least one buffer");
            assert!(! self.mru_index.is_empty(), "Invariant: we always have at least one index");

            let active_buf = self.mru_index.remove(0);
            self.buf_states.remove(active_buf);

            for idx in self.mru_index.iter_mut() {
                if *idx > active_buf {
                    *idx -= 1;
                }
            }
        }
    }
    
    fn ordered_buffer_paths(&self) -> Vec<String> {
        let mut paths = Vec::with_capacity(self.buf_states.len());

        for idx in self.mru_index.iter() {
            let buf_state = &self.buf_states[*idx];

            match buf_state.mod_or_file() {
                Some(ModuleOrFile::Module(_module)) => panic!("We don't handle module selection yet"),
                Some(ModuleOrFile::File(path)) => {
                    if path.as_os_str().is_empty() {
                        paths.push("Untitled".to_string());
                    } else {
                        paths.push(path.to_string_lossy().to_string());
                    }
                }
                None => paths.push("Untitled".to_string()),
            }
        }

        paths
    }
}

#[derive(Debug)]
struct BufferSelect {
    select_list_state: ListState,
    list: List<'static>,
    max_chars: usize,
}
impl BufferSelect {
    fn new(items: Vec<String>) -> Self {
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

    fn next(&mut self) {
        let len = self.list.len();
        // we control this via new()
        let cur_selected = self.select_list_state.selected().unwrap();

        let next = (cur_selected + 1) % len;
        self.select_list_state.select(Some(next as usize));
    }
    
    fn selected(&self) -> usize {
        // we control this via new()
        self.select_list_state.selected().unwrap()
    }

    fn max_width(&self) -> usize {
        self.max_chars + 5
    }
}
impl ChasmWidget for BufferSelect {
    fn handle_event(&mut self, _event: &Event, _ctx: &mut WidgetContext) {
        // BufferSelect doesn't handle any events itself, it just provides the UI for buffer selection
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let mut state = self.select_list_state;
        StatefulWidget::render(&self.list, area, buf, &mut state);
    }
}

pub (super) struct EditorPane {
    // Per-buffer state
    buffers: Buffers,
    buffer_select: Option<BufferSelect>,

    /// Different normal-mode input buffers.
    command: CommandBuffer,
    search: SearchBuffer,
    numeric: NumericBuffer,

    // Global yanked/deleted text
    content_register: Option<String>,
}
impl std::fmt::Debug for EditorPane {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Editor")
            .field("buffers", &self.buffers)
            .field("command", &self.command)
            .field("search", &self.search)
            .field("numeric", &self.numeric)
            .field("content_register", &self.content_register)
            .finish()
    }
}
impl EditorPane {
    pub (super) fn new(mod_or_file: ModuleOrFile) -> Self {
        Self {
            buffers: Buffers::new(vec![BufferState::new(mod_or_file)]),
            buffer_select: None,
            command: CommandBuffer::new(),
            search: SearchBuffer::new(),
            numeric: NumericBuffer::new(),
            content_register: None,
        }
    }

    #[cfg(test)]
    fn new_test() -> Self {
        use crate::code::editor::command::CommandBuffer;

        EditorPane {
            buffers: Buffers::new(vec![BufferState::new(ModuleOrFile::File(PathBuf::new()))]),
            buffer_select: None,
            command: CommandBuffer::new(),
            search: SearchBuffer::new(),
            numeric: NumericBuffer::new(),
            content_register: None,
        }
    }

    fn create_new_buffer(&mut self) {
        let new_buffer = BufferState::new(ModuleOrFile::File(PathBuf::new()));
        self.buffers.push(new_buffer);
    }

    fn process_editor_event<'a>(&mut self, event: &'a Event, ctx: &mut WidgetContext) -> Option<&'a Event> {
        let Event::Key(key_event) = event else {
            return Some(event)
        };

        match key_event.kind {
            KeyEventKind::Press => {
                match key_event.code {
                    KeyCode::Char('n') => {
                        if self.buffer_select.is_some() {
                            return None;
                        } else if key_event.modifiers == KeyModifiers::CONTROL {
                            self.create_new_buffer();
                            return None;
                        }
                    }
                    KeyCode::Tab => {
                        if key_event.modifiers == KeyModifiers::CONTROL {
                            let _select = self.buffer_select.get_or_insert_with(|| {
                                let selectable_buffers = self.buffers.ordered_buffer_paths();
                                BufferSelect::new(selectable_buffers)
                            });
                        }

                        if let Some(buffer_select) = &mut self.buffer_select {
                            buffer_select.next();
                            return None;
                        }
                    }
                    KeyCode::Esc => {
                        if self.buffer_select.is_some() {
                            self.buffer_select = None;
                            return None;
                        }
                    }
                    KeyCode::Enter => {
                        if let Some(buffer_select) = &mut self.buffer_select {
                            let selected = buffer_select.selected();
                            self.buffers.set_active(selected);
                            self.buffer_select = None;
                            return None;
                        }
                    }
                    _ => {
                        if self.buffer_select.is_some() {
                            return None;
                        }
                    }
                }
            }
            _ => {
                if self.buffer_select.is_some() {
                    return None;
                }
            }
        }

        // buffer select is inactive
        Some(event)
    }
}

impl ChasmWidget for EditorPane {
    fn handle_event(&mut self, event: &Event, ctx: &mut WidgetContext) {
        // check top-level editor events
        if let Some(unhandled_event) = self.process_editor_event(event, ctx) {
            let mut buf_ctx = BufferContext::new(
                &mut self.command,
                &mut self.search,
                &mut self.numeric,
                &mut self.content_register,
            );

            self.buffers.active_mut().update(unhandled_event, ctx, &mut buf_ctx);
        }
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let active_buffer = self.buffers.active();
        let height = area.height as usize;
        let code_len = active_buffer.code_lines().len();
        let digits_for_line_numbers = digits_in(code_len as u64);
        let total_lines = code_len;

        let layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(digits_for_line_numbers as u16 + 1), // line numbers + padding
                Constraint::Fill(1), // code
            ])
            .split(area);

        let gutter = layout[0];
        let code_area = layout[1];

        let start = active_buffer.scroll_y();
        let end = (start + height).min(total_lines);

        // render line numbers manually in gutter
        for (screen_row, line_idx) in (start..end).enumerate() {
            let line_number = line_idx + 1;

            let text = format!("{:<width$} ", line_number, width = digits_for_line_numbers as usize);

            let target_y = gutter.y + screen_row as u16;
            buf.set_string(
                gutter.x,
                target_y,
                text,
                Style::default().fg(Color::Gray),
            );
        }

        let code_slice = &active_buffer.code_lines()[start..end];
        let code_as_lines = code_slice.iter().map(|line| Line::from(line.as_str())).collect::<Vec<_>>();

        Paragraph::new(code_as_lines).block(Block::default()).render(code_area, buf);  

        // draw the line highlight (must be before selection)
        let mut cursor_line_is_visible = false;
        if (start..end).contains(&active_buffer.cursor_y()) {
            cursor_line_is_visible = true; 
            let cursor_screen_y = active_buffer.cursor_y() - start;
            let cursor_line_rect = Rect { y: code_area.y + cursor_screen_y as u16, x: gutter.x, width: gutter.width + code_area.width, height: 1 };
            buf.set_style(cursor_line_rect, Style::default().bg(Color::DarkGray));
        }


        // draw the selection
        let visible_range_y = start..end;

        if let Some(selection) = &active_buffer.active_selection() {
            if selection.is_empty() {
                // nothing to render
            } else if selection_has_no_overlap(selection, &visible_range_y) {
                // nothing to render
            } else {
                let (selection_start,selection_end) = selection_visible_range(selection, &visible_range_y);
                for line_no in selection_start.line..=selection_end.line {
                    let render_line = (line_no - start) as u16;

                    let (x_start, mut x_width) = if line_no == selection_start.line && line_no == selection_end.line {
                        // start at selection start, width is selection length
                        (selection_start.column as u16, (selection_end.column - selection_start.column) as u16)
                    } else if line_no == selection_start.line {
                        // start at selection start, width is entire line
                        (selection_start.column as u16, (active_buffer.code_lines()[line_no].len() - selection_start.column) as u16)
                    } else if line_no == selection_end.line {
                        // start at selection start, width is selection end column
                        (0, selection_end.column as u16) 
                    } else {
                        // entire line is selected
                        (0, active_buffer.code_lines()[line_no].len() as u16)
                    };

                    // This will visually account for:
                    // - the implied newline at the end of each line, or
                    // - the cursor's current position, or
                    // - the cursor's start position, when the selection moved backwards
                    x_width += 1;

                    let line_rect = Rect { y: code_area.y + render_line, x: code_area.x + x_start, width: x_width, height: 1 };
                    buf.set_style(line_rect, Style::default().bg(Color::Gray));
                }
            }
        }

        // draw cursor (after all highlights/selections)
        if cursor_line_is_visible {
            // TODO: we do nothing with x scroll
            if active_buffer.cursor_x() < code_area.width as usize {
                // cursor is within visible code area, draw normally
                let cursor_screen_y = active_buffer.cursor_y() - start;
                let cursor_line_rect_y = code_area.y + cursor_screen_y as u16;

                let cursor_screen_x = active_buffer.cursor_x() as u16 + gutter.width;
                let cell = buf.cell_mut((cursor_screen_x, cursor_line_rect_y)).unwrap();

                match active_buffer.mode() {
                    BufferMode::Normal => {
                        cell.set_style(Style::default().bg(Color::LightGreen).fg(Color::Black));
                    }
                    BufferMode::Insert => {
                        cell.set_style(Style::default().bg(Color::Red).fg(Color::Black));
                    }
                }
            }
        }

        // draw the buffer select overlay
        if let Some(buffer_select) = &self.buffer_select {
            let buffer_width = get_buffer_select_width(buffer_select.max_width() as u16, &area);

            let overlay_area = Rect {
                x: area.x,
                y: area.y,
                width: buffer_width as u16,
                height: area.height,
            };

            Clear.render(overlay_area, buf);

            let block = Block::default()
                .borders(Borders::RIGHT | Borders::BOTTOM)
                .style(Style::default().bg(Color::DarkGray));

            let inner = block.inner(overlay_area);
            block.render(overlay_area, buf);

            buffer_select.render(inner, buf);
        }
    }
}

/// Returns the recommended width of the buffer select based on the supplied area.
fn get_buffer_select_width(max_item_width: u16, area: &Rect) -> u16 {
    const MIN_WIDTH: u16 = 50;

    if max_item_width >= area.width {
        return area.width;
    }

    let base_width = MIN_WIDTH.max(max_item_width + 3);
    base_width.min(area.width)
}


fn selection_visible_range(selection: &VisualSelection, visible_range_y: &Range<usize>) -> (Position, Position) {
    assert!(!selection.is_empty(), "empty selection!");
    let (start, end) = selection_absolute_order(selection);
    clamp_selection_to_visible_range(start, end, visible_range_y)
}

fn clamp_selection_to_visible_range(start: Position, end: Position, visible_range_y: &Range<usize>) -> (Position, Position) {
    let clamped_start = if start.line < visible_range_y.start {
        Position { line: visible_range_y.start, column: 0 }
    } else {
        start
    };

    let clamped_end = if end.line >= visible_range_y.end {
        Position { line: visible_range_y.end - 1, column: usize::MAX }
    } else {
        end
    };

    (clamped_start, clamped_end)
}

/// Returns the (start, end) positions of a given selection by comparing their line/col positions.
fn selection_absolute_order(selection: &VisualSelection) -> (Position, Position) {
    if selection.anchor.line < selection.cursor.line {
        // selected downwards
        (selection.anchor, selection.cursor)
    } else if selection.cursor.line < selection.anchor.line {
        // selected upwards
        (selection.cursor, selection.anchor)
    } else if selection.anchor.column <= selection.cursor.column {
        // selection is across the same line
        (selection.anchor, selection.cursor)
    } else {
        assert!(selection.cursor.column < selection.anchor.column);
        (selection.cursor, selection.anchor)
    }
}

fn selection_has_no_overlap(selection: &VisualSelection, visible_range_y: &Range<usize>) -> bool {
    let anchor_is_above = selection.anchor.line < visible_range_y.start;
    let cursor_is_above = selection.cursor.line < visible_range_y.start;

    let anchor_is_below = selection.anchor.line >= visible_range_y.end;
    let cursor_is_below = selection.cursor.line >= visible_range_y.end;

    (anchor_is_above && cursor_is_above) || (anchor_is_below && cursor_is_below)
}

fn digits_in(n: u64) -> u32 {
    if n == 0 {
        1
    } else {
        n.ilog10() + 1
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct VisualSelection {
    anchor: Position,
    cursor: Position,
}
impl VisualSelection {
    fn is_empty(&self) -> bool {
        self.anchor == self.cursor
    }
    fn swap_anchor_and_cursor(&mut self) {
        std::mem::swap(&mut self.anchor, &mut self.cursor);
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Position {
    line: usize,
    column: usize,
}

#[derive(Clone, Debug)]
enum StringOrChar {
    String(String),
    Char(char),
}
impl From<String> for StringOrChar {
    fn from(s: String) -> Self {
        StringOrChar::String(s)
    }
}
impl From<char> for StringOrChar {
    fn from(c: char) -> Self {
        StringOrChar::Char(c)
    }
}
impl fmt::Display for StringOrChar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StringOrChar::String(s) => write!(f, "{s}"),
            StringOrChar::Char(c) => write!(f, "{c}"),
        }
    }
}

#[derive(Clone, Debug)]
enum Text {
    /// A string of text terminated with a newline
    Line(Vec<Span>),
    /// A string of text that does not end with a newline
    Span(Span),
}

#[derive(Clone, Debug)]
struct Span {
    content: String,
}
impl From<&str> for Span {
    fn from(s: &str) -> Self {
        Span::from(s.to_string())
    }
}
impl From<String> for Span {
    fn from(s: String) -> Self {
        Span { content: s }
    }
}

#[derive(Clone, Debug)]
enum Content {
    StringOrChar(StringOrChar),
    Lines(Vec<Text>),
}
impl From<String> for Content {
    fn from(s: String) -> Self {
        let mut multiline = false;
        let mut text_content = s.split_inclusive('\n')
            .into_iter()
            .map(|s| if s.ends_with('\n') {
                multiline = true;
                let stripped = strip_rn(s);
                Text::Line(vec![Span::from(stripped)])
            } else {
                Text::Span(Span::from(s))
            })
            .collect::<Vec<_>>();

        if multiline {
            Content::Lines(text_content)
        } else {
            let content = text_content.pop().unwrap();
            let Text::Span(span) = content else { panic!("Expected span since we asserted ! multiline") };
            Content::StringOrChar(span.content.into())
        }
    }
}
impl From<char> for Content {
    fn from(c: char) -> Self {
        Self::StringOrChar(c.into())
    }
}
impl fmt::Display for Content {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Content::StringOrChar(StringOrChar::String(s)) => write!(f, "{s}"),
            Content::StringOrChar(StringOrChar::Char(c)) => write!(f, "{c}"),
            Content::Lines(lines) => {
                let mut s = String::new();

                for text in lines.iter() {
                    match text {
                        Text::Span(span) => s.push_str(&span.content),
                        Text::Line(spans) => {
                            for span in spans.iter() {
                                s.push_str(&span.content);
                            }
                            s.push('\n');
                        },
                    }
                }

                write!(f, "{s}")
            }
        }
    }
}

fn strip_rn(s: &str) -> &str {
    let s = s.strip_suffix('\n').unwrap_or(s);
    let s = s.strip_suffix('\r').unwrap_or(s);
    s 
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn buffers_adds_buffer_state_when_provided_in_list() {
        let first_buf = BufferState::new(ModuleOrFile::File(PathBuf::new()));
        let first_id = first_buf.id();
        let second_buf = BufferState::new(ModuleOrFile::File(PathBuf::new()));

        let buffers = Buffers::new(vec![first_buf, second_buf]);

        assert_eq!(buffers.active().id(), first_id);
    }

    #[test]
    fn buffers_creates_buffer_state_when_no_buffers_provided() {
        let buffers = Buffers::new(vec![]);

        // compile check
        let _ = buffers.active().id();
    }

    #[test]
    fn buffers_adds_new_active_buffer_state_on_push() {
        let mut buffers = Buffers::new(vec![]);

        // Add first set
        let first_new_buf = BufferState::new(ModuleOrFile::File(PathBuf::new()));
        let first_new_id = first_new_buf.id();

        buffers.push(first_new_buf);

        let active_id = buffers.active().id();
        assert_eq!(first_new_id, active_id);

        // Add second set
        let second_new_buf = BufferState::new(ModuleOrFile::File(PathBuf::new()));
        let second_new_id = second_new_buf.id();

        buffers.push(second_new_buf);

        let active_id = buffers.active().id();
        assert_eq!(second_new_id, active_id);

        // Add third set
        let third_new_buf = BufferState::new(ModuleOrFile::File(PathBuf::new()));
        let third_new_id = third_new_buf.id();

        buffers.push(third_new_buf);

        let active_id = buffers.active().id();
        assert_eq!(third_new_id, active_id);

        assert_eq!(buffers.buf_states.len(), 4);
        assert_eq!(buffers.mru_index.len(), 4);

        assert_eq!(buffers.buf_states[1].id(), first_new_id);
        assert_eq!(buffers.buf_states[2].id(), second_new_id);
        assert_eq!(buffers.buf_states[3].id(), third_new_id);

        assert_eq!(buffers.mru_index, vec![3, 2, 1, 0]);
    }

    #[test]
    fn buffers_removes_active_buffer_state_on_remove_active() {
        let mut buffers = Buffers {
            buf_states: vec![
                BufferState::new(ModuleOrFile::File(PathBuf::new())), // next next active
                BufferState::new(ModuleOrFile::File(PathBuf::new())), // active
                BufferState::new(ModuleOrFile::File(PathBuf::new())), // next active
                BufferState::new(ModuleOrFile::File(PathBuf::new())), // last
            ],
            mru_index: vec![1, 2, 0, 3],
        };
        let mut ids = buffers.buf_states.iter().map(|b| b.id()).collect::<Vec<_>>();

        buffers.remove_active();

        assert_eq!(buffers.buf_states.len(), 3);
        assert_eq!(buffers.mru_index.len(), 3);

        ids.remove(1);
        assert_eq!(ids, buffers.buf_states.iter().map(|b| b.id()).collect::<Vec<_>>());
        assert_eq!(buffers.mru_index, vec![1, 0, 2]); // ids > removed id were decremented

        buffers.remove_active();

        assert_eq!(buffers.buf_states.len(), 2);
        assert_eq!(buffers.mru_index.len(), 2);

        ids.remove(1);
        assert_eq!(ids, buffers.buf_states.iter().map(|b| b.id()).collect::<Vec<_>>());
        assert_eq!(buffers.mru_index, vec![0, 1]);

        buffers.remove_active();

        assert_eq!(buffers.buf_states.len(), 1);
        assert_eq!(buffers.mru_index.len(), 1);

        ids.remove(0);
        assert_eq!(ids, buffers.buf_states.iter().map(|b| b.id()).collect::<Vec<_>>());
        assert_eq!(buffers.mru_index, vec![0]);

        buffers.remove_active();

        // last buffer is replaced with new empty buffer
        assert_eq!(buffers.buf_states.len(), 1);
        assert_eq!(buffers.mru_index.len(), 1);

        // make sure we don't still have the old id
        assert_ne!(ids, buffers.buf_states.iter().map(|b| b.id()).collect::<Vec<_>>());
        assert_eq!(buffers.mru_index, vec![0]);
    }
}