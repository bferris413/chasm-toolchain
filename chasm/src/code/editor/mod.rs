mod ops;
mod undo_redo;

use std::{iter::Peekable, ops::Range};

use crossterm::event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use ratatui::buffer::Buffer;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Style};
use ratatui::text::Line;
use ratatui::widgets::{Block, Paragraph, Widget};

use crate::code::editor::ops::DeleteXOp;
use crate::code::{AppCommand, ChasmWidget, Metadata};
use crate::project::ModulePath;
use ops::{DeleteBackOp, DeleteForwardOp, DeleteVisualOp, EditOp, InsertLineOp, InsertOp, InsertSingleOp, InsertVisualOp, SplitOp};
use undo_redo::UndoRedoStack;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum EditorMode {
    Normal,
    Insert,
}

#[derive(Debug)]
pub (super) struct Editor {
    module: ModulePath,
    code: Vec<String>,
    scroll_y: usize,
    mode: EditorMode,

    // cursor 0-based y position in the code (not screen)
    cursor_y: usize,
    // cursor 0-based x position in the code (not screen)
    cursor_x: usize,
    // last x position explicitly moved to by the user
    last_requested_x: usize,
    active_selection: Option<VisualSelection>,

    undo_redo: UndoRedoStack,
}
impl Editor {
    pub (super) fn new(module: ModulePath) -> Self {

        let code = std::fs::read_to_string(&module.path).unwrap_or_else(|e| {
            format!("Error loading module {}: {}\n\nReload or something...", module.name, e)
        });

        let code = code.lines().map(|line| line.to_string()).collect();

        Self {
            module,
            code,
            scroll_y: 0,
            mode: EditorMode::Normal,
            cursor_y: 0,
            cursor_x: 0,
            last_requested_x: 0,
            active_selection: None,
            undo_redo: UndoRedoStack::new(),
        }
    }

    fn snap_view_to_cursor(&mut self, meta: &Metadata) {
        let view_height = meta.view_area.0.height as usize;
        if self.cursor_y > self.scroll_y + view_height - 1 {
            self.scroll_y = self.cursor_y.saturating_sub(view_height - 1);
        } else if self.cursor_y < self.scroll_y {
            self.scroll_y = self.cursor_y;
        }
    }

    fn max_cursor_x(&self) -> usize {
        let clamp_at = 1;
        self.code[self.cursor_y].len().saturating_sub(clamp_at)
    }

    fn move_cursor_up(&mut self, move_size: usize, meta: &Metadata) {
        self.cursor_y = self.cursor_y.saturating_sub(move_size);
        self.cursor_x = self.last_requested_x.min(self.max_cursor_x());
        self.snap_view_to_cursor(meta);
    }

    fn move_cursor_down(&mut self, move_size: usize, meta: &Metadata) {
        self.cursor_y = self.cursor_y.saturating_add(move_size).min(self.code.len().saturating_sub(1));
        self.cursor_x = self.last_requested_x.min(self.max_cursor_x());
        self.snap_view_to_cursor(meta);
    }

    fn move_cursor_left(&mut self, distance: usize) {
        self.cursor_x = self.cursor_x.saturating_sub(distance);
        self.last_requested_x = self.cursor_x;
    }

    fn move_cursor_right(&mut self, distance: usize) {
        let max_cursor_x = self.max_cursor_x();
        self.cursor_x = (self.cursor_x + distance).min(max_cursor_x);
        self.last_requested_x = self.cursor_x;
    }

    fn delete_x(&mut self) -> Option<EditOp> {
        let old_pos = Position { line: self.cursor_y, column: self.cursor_x };
        if self.cursor_x < self.code[self.cursor_y].len() {
            let text = self.code[self.cursor_y].remove(self.cursor_x).into();
            let mut did_shift = false;
            if self.cursor_x == self.code[self.cursor_y].len() && !self.code[self.cursor_y].is_empty() {
                // we deleted the last character, move cursor left (Vim 'x' doesn't join lines)
                self.move_cursor_left(1);
                did_shift = true;
            }

            let mut new_pos = Position { line: self.cursor_y, column: self.cursor_x };
            if did_shift {
                new_pos.column += 1;
            }

            let edit = InsertSingleOp {
                at: Position { line: self.cursor_y, column: self.cursor_x },
                content: text,
                cursor_to: new_pos,
                cursor_from: old_pos,
            };

            Some(edit.into())
        } else {
            None
        }
    }

    fn delete_forward(&mut self) -> Option<EditOp> {
        if self.cursor_x < self.code[self.cursor_y].len() {
            let text = self.code[self.cursor_y].remove(self.cursor_x).into();

            let edit = InsertSingleOp {
                at: Position { line: self.cursor_y, column: self.cursor_x },
                content: text,
                cursor_to: Position { line: self.cursor_y, column: self.cursor_x },
                cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
            };

            Some(edit.into())
        } else if self.cursor_x == self.code[self.cursor_y].len() && self.cursor_y < self.code.len() - 1 {
            // delete the logical newline by merging the current line with the next line
            let line_to_append = self.code.remove(self.cursor_y + 1);
            self.code[self.cursor_y].push_str(&line_to_append);

            let edit = SplitOp {
                at: Position { line: self.cursor_y, column: self.cursor_x },
                cursor_to: Position { line: self.cursor_y, column: self.cursor_x },
                cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
            };

            Some(edit.into())
        } else {
            None
        }
    }

    fn delete_backward(&mut self) -> Option<EditOp> {
        if self.cursor_x > 0 {
            let c = self.code[self.cursor_y].remove(self.cursor_x - 1);
            self.cursor_x -= 1;
            self.last_requested_x = self.cursor_x;

            let inverse_op = InsertSingleOp {
                at: Position { line: self.cursor_y, column: self.cursor_x },
                content: c.into(),
                cursor_to: Position { line: self.cursor_y, column: self.cursor_x + 1 },
                cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
            };

            Some(inverse_op.into())
        } else if self.cursor_y > 0 {
            assert!(self.cursor_x == 0);

            let old_cursor = Position { line: self.cursor_y, column: self.cursor_x };
            let prev_line_len = self.code[self.cursor_y - 1].len();
            let current_line = self.code.remove(self.cursor_y);
            self.cursor_y -= 1;
            self.code[self.cursor_y].push_str(&current_line);
            self.cursor_x = prev_line_len;
            self.last_requested_x = self.cursor_x;
            let split_at = Position { line: self.cursor_y, column: self.cursor_x };

            let inverse_op = SplitOp {
                at: split_at,
                cursor_to: old_cursor,
                cursor_from: split_at,
            };

            Some(inverse_op.into())
        } else {
            None
        }
    }

    fn skip_x_whitespace_forward(&mut self) {
        let mut line_chars = self.code[self.cursor_y].chars();
        while let Some(c) = line_chars.next() {
            if c.is_ascii_whitespace() {
                self.cursor_x += 1;
            } else {
                break;
            }
        }
        self.last_requested_x = self.cursor_x;
    }

    fn toggle_selection(&mut self) {
        if self.active_selection.is_none() {
            let anchor = Position { line: self.cursor_y, column: self.cursor_x };
            self.active_selection = Some(VisualSelection { anchor, cursor: anchor });
        } else {
            self.active_selection = None;
        }
    }

    fn should_update_visual_selection(&self) -> bool {
        self.active_selection.is_some()
    }

    // TODO: refactor, my mental model for what this _should_ look like isn't clear yet
    fn visual_delete(&mut self, op: DeleteVisualOp, meta: &Metadata) -> Option<EditOp> {
        if op.selection.is_empty() {
            self.delete_forward()
        } else {
            let old_pos = Position { line: self.cursor_y, column: self.cursor_x };
            let (start, end) = selection_absolute_order(&op.selection);

            let removed_content = if start.line == end.line {
                // single line selection, delete the selected range
                let line_len = self.code[start.line].len();

                if end.column == line_len && end.line < self.code.len() - 1 {
                    // user is deleting the logical newline (like vim 'v$')

                    // delete the logical newline by merging with the next line
                    let line_to_append = self.code.remove(end.line + 1);
                    self.code[end.line].push_str(&line_to_append);

                    let line = &mut self.code[start.line];
                    let removed: String = line.drain(start.column..end.column).collect();

                    Content::Lines(Lines {
                        cur_line: removed.into(),
                        new_lines: vec![],
                        last_line: String::new().into(),
                        last_line_split: false,
                    })
                } else if end.column == self.code[start.line].len() {

                    let line = &mut self.code[start.line];
                    let removed: String = line.drain(start.column..end.column).collect();

                    Content::Lines(Lines {
                        cur_line: removed.into(),
                        new_lines: vec![],
                        last_line: String::new().into(),
                        last_line_split: false,
                    })

                } else {
                    let line = &mut self.code[start.line];
                    let removed: String = line.drain(start.column..=end.column).collect();
                    Content::StringOrChar(removed.into())

                }

            } else {
                // multi-line selection, delete the selected range and merge lines
                let splice_start = if self.code[start.line].is_empty() { start.line } else { start.line + 1 };

                let first_line = &mut self.code[start.line];
                let mut first_line_removed = String::new();
                if ! first_line.is_empty() {
                    first_line_removed = first_line.drain(start.column..).collect();
                }

                let last_line_len = self.code[end.line].len();
                let mut last_line_removed = String::new();
                let mut last_line_split = false;

                if last_line_len != 0 {
                    if end.column == last_line_len && end.line < self.code.len() - 1 {
                        // user is deleting the logical newline (like vim 'v$')

                        // delete the logical newline by merging with the next line
                        let line_to_append = self.code.remove(end.line + 1);
                        self.code[end.line].push_str(&line_to_append);

                        last_line_removed = self.code[end.line].drain(..end.column).collect();
                        last_line_split = true;
                    } else if end.column == last_line_len {
                        last_line_removed = self.code[end.line].drain(..end.column).collect();
                        last_line_split = true;
                    } else {
                        last_line_removed = self.code[end.line].drain(..=end.column).collect();
                    }
                }

                // remove all lines between start and end
                let spliced_lines = if end.line == splice_start {
                    vec![]
                } else {
                    self.code.splice(splice_start..end.line, std::iter::empty()).collect()
                };

                // merge first and last line
                let last_line = self.code.remove(start.line + 1);
                self.code[start.line].push_str(&last_line);

                if self.code[start.line].is_empty() && self.code.len() > 1 {
                    self.code.remove(start.line);
                    last_line_split = true;
                }

                Content::Lines(Lines {
                    cur_line: first_line_removed.into(),
                    new_lines: spliced_lines,
                    last_line: last_line_removed.into(),
                    last_line_split,
                })
            };

            let target_y = start.line;
            let y_distance = self.cursor_y.saturating_sub(target_y);
            self.move_cursor_up(y_distance, meta);

            let target_x = start.column;
            if self.cursor_x > target_x {
                let x_distance = self.cursor_x.saturating_sub(target_x);
                self.move_cursor_left(x_distance);
            } else {
                let x_distance = target_x.saturating_sub(self.cursor_x);
                self.move_cursor_right(x_distance); 
            }

            let inverse_op = InsertVisualOp {
                selection: op.selection,
                content: removed_content,
                cursor_to: Position { line: self.cursor_y, column: self.cursor_x },
                cursor_from: old_pos,
            };

            Some(inverse_op.into())
        }

    }


    fn skip_until_eol_or(&mut self, pred: fn(char) -> bool) {
        let current_line = &self.code[self.cursor_y];
        let mut chars = current_line.chars().enumerate().skip(self.cursor_x);

        while let Some((idx, c)) = chars.next() {
            if pred(c) {
                break;
            } else {
                self.cursor_x = idx;
                self.last_requested_x = self.cursor_x;
            }
        }
    }

    fn skip_until_line_start_or(&mut self, pred: fn(char) -> bool) {
        let current_line = &self.code[self.cursor_y];
        let line_len = current_line.len();
        let mut chars = current_line.chars().rev().enumerate().skip(line_len - self.cursor_x);

        while let Some((idx, c)) = chars.next() {
            if pred(c) {
                break;
            } else {
                self.cursor_x = line_len - idx - 1;
                self.last_requested_x = self.cursor_x;
            }
        }
    }

    fn move_to_next_word(&mut self, meta: &Metadata) {
        let current_line = &self.code[self.cursor_y];
        let mut chars = current_line.chars().enumerate().skip(self.cursor_x);

        // skip current word if we're in the middle of it
        if let Some((_, c)) = chars.next() {
            if !c.is_ascii_whitespace() {
                while let Some((_, c)) = chars.next() {
                    if c.is_ascii_whitespace() {
                        break;
                    }
                }
            }
        }

        // skip whitespace to the start of the next word
        while let Some((idx, c)) = chars.next() {
            if !c.is_ascii_whitespace() {
                self.cursor_x = idx;
                self.last_requested_x = self.cursor_x;
                return;
            }
        }

        // move to the start of the next line if we hit EOL
        if self.cursor_y < self.code.len() - 1 {
            self.move_cursor_down(1, meta);
            self.cursor_x = 0;
            self.skip_x_whitespace_forward();
        }
    }

    fn move_to_next_word_end(&mut self, meta: &Metadata) {
        fn is_whitespace(c: char) -> bool {
            c.is_ascii_whitespace()
        }

        let current_line = &self.code[self.cursor_y];
        let mut chars = current_line.chars().enumerate().skip(self.cursor_x).peekable();

        // skip to the end of the current word if we're in the middle of one
        if let Some((_, c)) = chars.next() {
            if !(c.is_ascii_whitespace() || is_word_boundary(&mut chars)) {
                // we're in a word and it's not the last character
                self.skip_until_eol_or(is_whitespace);
                return;
            }
        }

        // otherwise skip to the next word, then move to the end of it
        self.move_to_next_word(meta);
        self.skip_until_eol_or(is_whitespace);
    }

    fn move_to_previous_word(&mut self, meta: &Metadata) {
        let current_line = &self.code[self.cursor_y];
        let line_len = current_line.len();
        let mut chars = current_line.chars().rev().enumerate().skip(line_len - self.cursor_x);

        // skip current word if we're in the middle of it
        if let Some((_, c)) = chars.next() {
            if !c.is_ascii_whitespace() {
                while let Some((_, c)) = chars.next() {
                    if c.is_ascii_whitespace() {
                        break;
                    }
                }
            }
        }

        // skip whitespace to the start of the next word
        while let Some((idx, c)) = chars.next() {
            if !c.is_ascii_whitespace() {
                self.cursor_x = line_len - idx - 1;
                self.last_requested_x = self.cursor_x;
                return;
            }
        }

        // move to the end of the prev line if we hit line start
        if self.cursor_y > 0 {
            self.move_cursor_up(1, meta);
            self.cursor_x = self.max_cursor_x();
            self.last_requested_x = self.cursor_x;
        }
    }

    fn move_to_previous_word_start(&mut self, meta: &Metadata) {
        fn is_whitespace(c: char) -> bool {
            c.is_ascii_whitespace()
        }

        let current_line = &self.code[self.cursor_y];
        let line_len = current_line.len();
        let mut chars = current_line.chars().rev().enumerate().skip(line_len - self.cursor_x).peekable();

        // skip to beginning of current word if we're in the middle of one
        if let Some((_, c)) = chars.next() {
            if !(c.is_ascii_whitespace() || is_word_boundary(&mut chars)) {
                // we're in a word and it's not the last character
                self.skip_until_line_start_or(is_whitespace);
                return;
            }
        }

        // otherwise skip to the previous word, then move to the beginning of it
        self.move_to_previous_word(meta);
        self.skip_until_line_start_or(is_whitespace);
    }

    fn move_to(&mut self, pos: Position, meta: &Metadata) {
        self.cursor_y = pos.line;
        self.cursor_x = pos.column;
        self.cursor_x = self.cursor_x.min(self.max_cursor_x());
        self.last_requested_x = self.cursor_x;
        self.snap_view_to_cursor(meta);
    }

    /// Applies the edit operation and returns the inverse *if* something was performed.
    fn apply(&mut self, op: EditOp, meta: &Metadata) -> Option<EditOp> {
        match op {
            EditOp::Insert(insert_op) => {
                let inverse_op = insert_op.clone().invert();
                let current_line = &mut self.code[insert_op.at.line];
                let col = insert_op.at.column;

                match insert_op.content {
                    StringOrChar::String(ref s) => {
                        current_line.insert_str(col, s);
                    }
                    StringOrChar::Char(c) => {
                        current_line.insert(col, c);
                    }
                }

                self.move_to(insert_op.cursor_to, meta);
                Some(inverse_op.into())
            }
            EditOp::Delete(delete_op) => {
                let inverse_op = delete_op.clone().invert();

                let current_line = &mut self.code[delete_op.at.line];
                let col = delete_op.at.column;

                match delete_op.content.unwrap() {
                    StringOrChar::String(ref s) => {
                        current_line.replace_range(col..col + s.len(), "");
                    }
                    StringOrChar::Char(_) => {
                        current_line.remove(col);
                    }
                }

                self.move_to(delete_op.cursor_to, meta);
                Some(inverse_op.into())

            }
            EditOp::InsertSingle(insert_single_op) => {
                let inverse_op = insert_single_op.clone().invert();

                let current_line = &mut self.code[insert_single_op.at.line];
                let col = insert_single_op.at.column;

                let c = insert_single_op.content;
                current_line.insert(col, c);

                self.move_to(insert_single_op.cursor_to, meta);
                Some(inverse_op.into())
            }
            EditOp::InsertVisual(insert_visual_op) => {
                let inverse_op = insert_visual_op.clone().invert();

                let (start, _end) = selection_absolute_order(&insert_visual_op.selection);
                let current_line = &mut self.code[start.line];
                let col = start.column;

                match insert_visual_op.content {
                    Content::Lines(mut lines) => {
                        let mut last_line = current_line.split_off(col);

                        match lines.cur_line {
                            StringOrChar::String(ref s) => current_line.push_str(s),
                            StringOrChar::Char(c) => current_line.push(c),
                        }

                        let last_line = match lines.last_line {
                            StringOrChar::String(mut s) => {
                                if lines.last_line_split {
                                    s.push_str("\n");
                                }

                                s.push_str(&last_line);
                                s
                            }
                            StringOrChar::Char(c) => {
                                last_line.insert(0, c);

                                if lines.last_line_split {
                                    last_line.insert(1, '\n');
                                }

                                last_line
                            }
                        };

                        let mut last_lines = last_line.split('\n').collect::<Vec<_>>();
                        while let Some(line) = last_lines.pop() {
                            self.code.insert(start.line + 1, line.to_string());
                        }

                        // self.code.insert(start.line + 1, last_line);

                        while let Some(line) = lines.new_lines.pop() {
                            self.code.insert(start.line + 1, line);
                        }
                    }
                    Content::StringOrChar(StringOrChar::String(s)) => {
                        current_line.insert_str(col, &s);
                    }
                    Content::StringOrChar(StringOrChar::Char(c)) => {
                        current_line.insert(col, c);
                    }
                }
                
                self.move_to(insert_visual_op.cursor_to, meta);
                Some(inverse_op.into())
            }
            EditOp::InsertLine(insert_line_op) => {
                let inverse_op = insert_line_op.clone().invert();

                self.code.insert(insert_line_op.y_pos, insert_line_op.content);
                self.move_to(insert_line_op.cursor_to, meta);
                Some(inverse_op.into())
            }
            EditOp::DeleteLine(delete_line_op) => {
                let inverse_op = delete_line_op.clone().invert();

                self.code.remove(delete_line_op.y_pos);
                self.move_to(delete_line_op.cursor_to, meta);
                Some(inverse_op.into())
            }
            EditOp::Split(split_op) => {
                let line = &mut self.code[split_op.at.line];
                let new_line = line.split_off(split_op.at.column);
                self.code.insert(split_op.at.line + 1, new_line);
                self.move_to(split_op.cursor_to, meta);

                let inverse = split_op.invert();
                Some(inverse.into())
            }
            EditOp::Join(join_op) => {
                let current_line = self.code.remove(join_op.at.line + 1);
                let line = &mut self.code[join_op.at.line];
                line.push_str(&current_line);
                self.move_to(join_op.cursor_to, meta);

                let inverse = join_op.invert();
                Some(inverse.into())
            }
            EditOp::DeleteBack(_delete_op) => {
                let inverse_op = self.delete_backward();
                inverse_op
            }
            EditOp::DeleteForward(_delete_op) => {
                let inverse_op = self.delete_forward();
                inverse_op
            }
            EditOp::DeleteX(_delete_op) => {
                let inverse_op = self.delete_x();
                inverse_op
            }
            EditOp::DeleteVisual(delete_visual_op) => {
                let inverse_op = self.visual_delete(delete_visual_op, meta);
                inverse_op
            },
        }
    }

    fn handle_normal_mode_event(&mut self, event: &Event, meta: &Metadata) -> AppCommand {
        fn get_vertical_move_distance(kev: &KeyEvent, meta: &Metadata) -> usize {
            if kev.modifiers.contains(KeyModifiers::CONTROL) {
                (meta.view_area.0.height as usize).saturating_sub(1)
            } else {
                1
            }
        }

        if ! event.is_key_press() {
            return AppCommand::None;
        }

        let Event::Key(key_event) = event else { unreachable!() };

        let app_cmd = match key_event.code {
            KeyCode::Esc => {
                if self.active_selection.is_some() {
                    self.active_selection = None;
                }
                AppCommand::None
            }
            KeyCode::Char('G') => {
                let target_line = self.code.len().saturating_sub(1);
                if target_line != self.cursor_y {
                    let move_len = target_line.saturating_sub(self.cursor_y);
                    self.move_cursor_down(move_len, meta);
                }
                AppCommand::None
            }
            KeyCode::Char('g') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                let target_line = 0;
                if target_line != self.cursor_y {
                    let move_len = self.cursor_y.saturating_sub(target_line);
                    self.move_cursor_up(move_len, meta);
                }

                AppCommand::None
            }
            KeyCode::Char('I') => {
                self.cursor_x = 0;
                self.skip_x_whitespace_forward();
                self.mode = EditorMode::Insert;
                self.active_selection = None;
                AppCommand::None
            }
            KeyCode::Char('x') => {
                if let Some(selection) = self.active_selection.take() {
                    let visual_delete_op = DeleteVisualOp {
                        selection,
                        deleted: None,
                    };
                    
                    if let Some(inverse_op) = self.apply(visual_delete_op.into(), meta) {
                        self.undo_redo.push(inverse_op);
                    }
                } else {
                    let edit = DeleteXOp {
                        at: Position { line: self.cursor_y, column: self.cursor_x },
                        content: None,
                        cursor_to: Position { line: self.cursor_y, column: self.cursor_x },
                        cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
                    };

                    if let Some(inverse_op) = self.apply(edit.into(), meta) {
                        self.undo_redo.push(inverse_op);
                    }
                }

                AppCommand::None
            }
            KeyCode::Char('d') => {
                if key_event.modifiers.contains(KeyModifiers::CONTROL) {
                    let move_size = (meta.view_area.0.height as usize).saturating_sub(1);
                    self.move_cursor_down(move_size, meta);
                } else if let Some(selection) = self.active_selection.take() {
                    let visual_delete = DeleteVisualOp {
                        selection,
                        deleted: None,
                    };

                    if let Some(inverse_op) = self.apply(visual_delete.into(), meta) {
                        self.undo_redo.push(inverse_op);
                    }
                }

                AppCommand::None
            }
            KeyCode::Char('A') => {
                let is_max_normal_len = self.cursor_x == self.max_cursor_x();
                let is_empty = self.code[self.cursor_y].is_empty();

                if is_empty {
                    assert!(is_max_normal_len);
                } else {
                    let line_len = self.code[self.cursor_y].len();
                    let distance = line_len.saturating_sub(self.cursor_x);
                    self.move_cursor_right(distance);
                    self.cursor_x += 1; // appending
                }

                self.mode = EditorMode::Insert;
                AppCommand::None

            }
            KeyCode::Char('a') => {
                if self.active_selection.is_none() {
                    let is_max_normal_len = self.cursor_x == self.max_cursor_x();
                    let is_empty = self.code[self.cursor_y].is_empty();

                    if is_empty {
                        // we have an empty line, no need to move cursor
                        assert!(is_max_normal_len);
                    } else if is_max_normal_len {
                        // user wants to append
                        self.cursor_x += 1;
                    } else {
                        self.move_cursor_right(1);
                    }

                    self.mode = EditorMode::Insert;
                }

                AppCommand::None
            }
            KeyCode::Char('s') => {
                if let Some(selection) = self.active_selection.take() {
                    let visual_delete = DeleteVisualOp {
                        selection,
                        deleted: None,
                    };

                    if let Some(inverse_op) = self.apply(visual_delete.into(), meta) {
                        self.undo_redo.push(inverse_op);
                    }
                } else {
                    let is_empty = self.code[self.cursor_y].is_empty();

                    if !is_empty {
                        let delete_forward = DeleteForwardOp {
                            at: Position { line: self.cursor_y, column: self.cursor_x },
                            cursor_to: Position { line: self.cursor_y, column: self.cursor_x },
                            cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
                            content: None,
                        };

                        if let Some(undo_op) = self.apply(delete_forward.into(), meta) {
                            self.undo_redo.push(undo_op);
                        }
                    }
                }

                self.mode = EditorMode::Insert;
                AppCommand::None
            }
            KeyCode::Char('O') => {
                if let Some(selection) = self.active_selection.as_mut() {
                    selection.swap_anchor_and_cursor(); 
                    self.cursor_x = selection.cursor.column;
                    self.cursor_y = selection.cursor.line;
                    self.last_requested_x = self.cursor_x;
                    self.snap_view_to_cursor(meta);
                } else {
                    let has_no_lines = self.code.is_empty();

                    let insert_op = if has_no_lines {
                        InsertLineOp {
                            y_pos: 0,
                            content: String::new(),
                            cursor_to: Position { line: 0, column: 0 },
                            cursor_from: Position { line: 0, column: 0 },
                        }.into()
                    } else {
                        InsertLineOp {
                            y_pos: self.cursor_y,
                            content: String::new(),
                            cursor_to: Position { line: self.cursor_y, column: 0 },
                            cursor_from: Position { line: self.cursor_y, column: 0 },
                        }.into()
                    };

                    let inverse_op = self.apply(insert_op, meta).unwrap();
                    self.undo_redo.push(inverse_op);

                    self.mode = EditorMode::Insert;
                }

                AppCommand::None
            }
            KeyCode::Char('o') => {
                if let Some(selection) = self.active_selection.as_mut() {
                    selection.swap_anchor_and_cursor(); 
                    self.cursor_x = selection.cursor.column;
                    self.cursor_y = selection.cursor.line;
                    self.last_requested_x = self.cursor_x;
                    self.snap_view_to_cursor(meta);
                } else {
                    let insert_line_op = InsertLineOp {
                        y_pos: self.cursor_y + 1,
                        content: String::new(),
                        cursor_to: Position { line: self.cursor_y + 1, column: 0 },
                        cursor_from: Position { line: self.cursor_y, column: 0 },
                    };

                    let inverse_op = self.apply(insert_line_op.into(), meta).unwrap();
                    self.undo_redo.push(inverse_op);

                    self.mode = EditorMode::Insert;
                }

                AppCommand::None
            }
            KeyCode::Char('J') | KeyCode::Down if key_event.modifiers.contains(KeyModifiers::SHIFT) => {
                self.scroll_y = self.scroll_y.saturating_add(1);
                AppCommand::None
            }
            KeyCode::Char('K') | KeyCode::Up if key_event.modifiers.contains(KeyModifiers::SHIFT) => {
                self.scroll_y = self.scroll_y.saturating_sub(1);
                AppCommand::None
            }
            KeyCode::Char('u') => {
                if key_event.modifiers.contains(KeyModifiers::CONTROL) {
                    let move_size = (meta.view_area.0.height as usize).saturating_sub(1);
                    self.move_cursor_up(move_size, meta);
                    AppCommand::None
                } else {
                    if let Some(op) = self.undo_redo.undo() {
                        self.apply(op, meta);
                    }
                    AppCommand::None
                }

            }
            KeyCode::Char('r') => {
                if ! key_event.modifiers.contains(KeyModifiers::CONTROL) {
                    return AppCommand::None;
                }

                if let Some(op) = self.undo_redo.redo() {
                    self.apply(op, meta);
                }

                AppCommand::None
            }
            KeyCode::Char('j') | KeyCode::Down => {
                let move_size = get_vertical_move_distance(key_event, meta);
                self.move_cursor_down(move_size, meta);
                AppCommand::None
            }
            KeyCode::Char('k') | KeyCode::Up => {
                let move_size = get_vertical_move_distance(key_event, meta);
                self.move_cursor_up(move_size, meta);
                AppCommand::None
            }
            KeyCode::Char('h') | KeyCode::Left => {
                self.move_cursor_left(1);
                AppCommand::None
            }
            KeyCode::Char('l') | KeyCode::Right => {
                self.move_cursor_right(1);
                AppCommand::None
            }
            KeyCode::Char('0') => {
                self.cursor_x = 0;
                self.last_requested_x = self.cursor_x;
                AppCommand::None
            }
            KeyCode::Char('$') => {
                let max_cursor_x = self.max_cursor_x();
                self.cursor_x = max_cursor_x;

                if self.active_selection.is_some() && ! self.code[self.cursor_y].is_empty()  {
                    // vim 'v$' includes the logical newline if we're not on an empty line
                    self.cursor_x += 1;
                }

                self.last_requested_x = self.cursor_x;
                AppCommand::None
            }
            KeyCode::Char('w') => {
                self.move_to_next_word(meta);
                AppCommand::None
            }
            KeyCode::Char('e') => {
                self.move_to_next_word_end(meta);
                AppCommand::None
            }
            KeyCode::Char('b') => {
                self.move_to_previous_word_start(meta);
                AppCommand::None
            }
            KeyCode::Char('i') => {
                if self.active_selection.is_none() {
                    self.mode = EditorMode::Insert;
                }
                AppCommand::None
            }
            KeyCode::Char('v') => {
                self.toggle_selection();
                AppCommand::None
            }
            _other => {
                AppCommand::None
            }
        };

        let new_cursor_pos = Position { line: self.cursor_y, column: self.cursor_x };

        if self.should_update_visual_selection() {
            let selection = self.active_selection.as_mut().unwrap();
            selection.cursor = new_cursor_pos;
        }

        app_cmd
    }

    fn handle_insert_mode_event(&mut self, event: &Event, meta: &Metadata) -> AppCommand {
        if let Event::Key(key_event) = event && key_event.kind == KeyEventKind::Press {
            match key_event.code {
                KeyCode::Esc => {
                    self.mode = EditorMode::Normal;
                    let is_empty = self.code[self.cursor_y].is_empty();
                    let is_off_line = self.cursor_x == self.code[self.cursor_y].len();

                    if is_empty && is_off_line {
                        // nothing to do, cursor is in valid position
                    } else if is_off_line {
                        assert!(!is_empty);
                        // we are on a non-empty line but past the end (e.g. via 'a'), move back to last char
                        self.move_cursor_left(1);
                    }

                    self.last_requested_x = self.cursor_x;
                    AppCommand::None
                }
                KeyCode::Char('j') | KeyCode::Char('k') | KeyCode::Char('h') | KeyCode::Char('l') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                    match key_event.code {
                        KeyCode::Char('j') => {
                            self.move_cursor_down(1, meta);
                        }
                        KeyCode::Char('k') => {
                            self.move_cursor_up(1, meta);
                        }
                        KeyCode::Char('h') => {
                            self.move_cursor_left(1);
                        }
                        KeyCode::Char('l') => {
                            self.move_cursor_right(1);
                        }
                        _ => unreachable!(),
                    }

                    AppCommand::None
                }
                KeyCode::Left => {
                    self.move_cursor_left(1);
                    AppCommand::None
                }
                KeyCode::Right => {
                    self.move_cursor_right(1);
                    AppCommand::None
                }
                KeyCode::Up => {
                    self.move_cursor_up(1, meta);
                    AppCommand::None
                }
                KeyCode::Down => {
                    self.move_cursor_down(1, meta);
                    AppCommand::None
                }
                KeyCode::Char(c) => {
                    let insert_op = InsertSingleOp {
                        at: Position { line: self.cursor_y, column: self.cursor_x },
                        content: c.into(),
                        cursor_to: Position { line: self.cursor_y, column: self.cursor_x + 1 },
                        cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
                    };

                    let undo_op = self.apply(insert_op.into(), meta).unwrap();
                    self.undo_redo.push(undo_op);

                    AppCommand::None
                }
                KeyCode::Backspace => {
                    let delete_back = DeleteBackOp {
                        at: Position { line: self.cursor_y, column: self.cursor_x },
                        cursor_to: Position { line: self.cursor_y, column: self.cursor_x.saturating_sub(1) },
                        cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
                        content: None,
                    };
                    if let Some(undo_op) = self.apply(delete_back.into(), meta) {
                        self.undo_redo.push(undo_op);
                    }

                    AppCommand::None
                }
                KeyCode::Delete => {
                    let delete_forward = DeleteForwardOp {
                        at: Position { line: self.cursor_y, column: self.cursor_x },
                        cursor_to: Position { line: self.cursor_y, column: self.cursor_x },
                        cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
                        content: None,
                    };

                    if let Some(undo_op) = self.apply(delete_forward.into(), meta) {
                        self.undo_redo.push(undo_op);
                    }

                    AppCommand::None
                }
                KeyCode::Enter => {
                    let split_op = SplitOp {
                        at: Position { line: self.cursor_y, column: self.cursor_x },
                        cursor_to: Position { line: self.cursor_y + 1, column: 0 },
                        cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
                    };

                    let undo_op = self.apply(split_op.into(), meta).unwrap();
                    self.undo_redo.push(undo_op);

                    AppCommand::None
                }
                KeyCode::Tab => {
                    let content = " ".repeat(4);
                    let len = content.len();

                    let insert_op = InsertOp {
                        at: Position { line: self.cursor_y, column: self.cursor_x },
                        content: StringOrChar::String(content),
                        cursor_to: Position { line: self.cursor_y, column: self.cursor_x + len },
                        cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
                    };

                    let undo_op = self.apply(insert_op.into(), meta).unwrap();
                    self.undo_redo.push(undo_op);

                    AppCommand::None
                }
                _other => {
                    AppCommand::None
                }
            }
        } else {
            AppCommand::None
        }
    }

}
impl ChasmWidget for Editor {
    
    fn handle_event(&mut self, event: &Event, meta: &Metadata) -> AppCommand {
        match self.mode {
            EditorMode::Normal => self.handle_normal_mode_event(event, meta),
            EditorMode::Insert => self.handle_insert_mode_event(event, meta),
        }
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let height = area.height as usize;
        let digits_for_line_numbers = digits_in(self.code.len() as u64);
        let total_lines = self.code.len();

        let layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(digits_for_line_numbers as u16 + 1), // line numbers + padding
                Constraint::Fill(1), // code
            ])
            .split(area);

        let gutter = layout[0];
        let code_area = layout[1];

        let start = self.scroll_y;
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

        let code_slice = &self.code[start..end];
        let code_as_lines = code_slice.iter().map(|line| Line::from(line.as_str())).collect::<Vec<_>>();

        Paragraph::new(code_as_lines).block(Block::default()).render(code_area, buf);  

        // draw the line highlight (must be before selection)
        let mut cursor_line_is_visible = false;
        if (start..end).contains(&self.cursor_y) {
            cursor_line_is_visible = true; 
            let cursor_screen_y = self.cursor_y - start;
            let cursor_line_rect = Rect { y: code_area.y + cursor_screen_y as u16, x: gutter.x, width: gutter.width + code_area.width, height: 1 };
            buf.set_style(cursor_line_rect, Style::default().bg(Color::DarkGray));
        }


        // draw the selection
        let visible_range_y = start..end;

        if let Some(selection) = &self.active_selection {
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
                        (selection_start.column as u16, (self.code[line_no].len() - selection_start.column) as u16)
                    } else if line_no == selection_end.line {
                        // start at selection start, width is selection end column
                        (0, selection_end.column as u16) 
                    } else {
                        // entire line is selected
                        (0, self.code[line_no].len() as u16)
                    };

                    // This will visually account for:
                    // - the invisible-but-implied newline at the end of each line, or
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
            // TODO: we do nothing we x scroll
            if self.cursor_x < code_area.width as usize {
                // cursor is within visible code area, draw normally
                let cursor_screen_y = self.cursor_y - start;
                let cursor_line_rect_y = code_area.y + cursor_screen_y as u16;

                let cursor_screen_x = self.cursor_x as u16 + gutter.width;
                let cell = buf.cell_mut((cursor_screen_x, cursor_line_rect_y)).unwrap();

                match self.mode {
                    EditorMode::Normal => {
                        cell.set_style(Style::default().bg(Color::LightGreen).fg(Color::Black));
                    }
                    EditorMode::Insert => {
                        cell.set_style(Style::default().bg(Color::Red).fg(Color::Black));
                    }
                }
            }
        }
    }
}

fn is_word_boundary(chars: &mut Peekable<impl Iterator<Item = (usize, char)>>) -> bool {
    if let Some((_, next_c)) = chars.peek() {
        next_c.is_ascii_whitespace()
    } else {
        true
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

#[derive(Clone, Debug)]
struct Lines {
    cur_line: StringOrChar,
    new_lines: Vec<String>,
    last_line: StringOrChar,
    last_line_split: bool,
}

#[derive(Clone, Debug)]
enum Content {
    StringOrChar(StringOrChar),
    Lines(Lines),
}
impl From<String> for Content {
    fn from(s: String) -> Self {
        Self::StringOrChar(s.into())
    }
}
impl From<char> for Content {
    fn from(c: char) -> Self {
        Self::StringOrChar(c.into())
    }
}
impl From<Lines> for Content {
    fn from(lines_insert: Lines) -> Self {
        Self::Lines(lines_insert)
    }
}