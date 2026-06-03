mod buffer;
mod command;
mod numeric;
mod ops;
mod search;
mod undo_redo;

use std::{fmt, vec};
use std::path::PathBuf;
use std::ops::Range;

use crossterm::event::Event;
use ratatui::buffer::Buffer;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Style};
use ratatui::text::Line;
use ratatui::widgets::{Block, Paragraph, Widget};

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

pub (super) struct EditorPane {
    // Per-buffer state
    buf_state: BufferState,

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
            .field("buf_state", &self.buf_state)
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
            buf_state: BufferState::new(mod_or_file),
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
            buf_state: BufferState::new(ModuleOrFile::File(PathBuf::new())),
            command: CommandBuffer::new(),
            search: SearchBuffer::new(),
            numeric: NumericBuffer::new(),
            content_register: None,
        }
    }
}

impl ChasmWidget for EditorPane {
    fn handle_event(&mut self, event: &Event, ctx: &mut WidgetContext) {
        let mut buf_ctx = BufferContext::new(
            &mut self.command,
            &mut self.search,
            &mut self.numeric,
            &mut self.content_register,
        );

        self.buf_state.update(event, ctx, &mut buf_ctx);
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let height = area.height as usize;
        let code_len = self.buf_state.code_lines().len();
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

        let start = self.buf_state.scroll_y();
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

        let code_slice = &self.buf_state.code_lines()[start..end];
        let code_as_lines = code_slice.iter().map(|line| Line::from(line.as_str())).collect::<Vec<_>>();

        Paragraph::new(code_as_lines).block(Block::default()).render(code_area, buf);  

        // draw the line highlight (must be before selection)
        let mut cursor_line_is_visible = false;
        if (start..end).contains(&self.buf_state.cursor_y()) {
            cursor_line_is_visible = true; 
            let cursor_screen_y = self.buf_state.cursor_y() - start;
            let cursor_line_rect = Rect { y: code_area.y + cursor_screen_y as u16, x: gutter.x, width: gutter.width + code_area.width, height: 1 };
            buf.set_style(cursor_line_rect, Style::default().bg(Color::DarkGray));
        }


        // draw the selection
        let visible_range_y = start..end;

        if let Some(selection) = &self.buf_state.active_selection() {
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
                        (selection_start.column as u16, (self.buf_state.code_lines()[line_no].len() - selection_start.column) as u16)
                    } else if line_no == selection_end.line {
                        // start at selection start, width is selection end column
                        (0, selection_end.column as u16) 
                    } else {
                        // entire line is selected
                        (0, self.buf_state.code_lines()[line_no].len() as u16)
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
            if self.buf_state.cursor_x() < code_area.width as usize {
                // cursor is within visible code area, draw normally
                let cursor_screen_y = self.buf_state.cursor_y() - start;
                let cursor_line_rect_y = code_area.y + cursor_screen_y as u16;

                let cursor_screen_x = self.buf_state.cursor_x() as u16 + gutter.width;
                let cell = buf.cell_mut((cursor_screen_x, cursor_line_rect_y)).unwrap();

                match self.buf_state.mode() {
                    BufferMode::Normal => {
                        cell.set_style(Style::default().bg(Color::LightGreen).fg(Color::Black));
                    }
                    BufferMode::Insert => {
                        cell.set_style(Style::default().bg(Color::Red).fg(Color::Black));
                    }
                }
            }
        }
    }
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