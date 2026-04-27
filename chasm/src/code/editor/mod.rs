mod ops;
mod search;
mod undo_redo;

use std::{fmt, vec};
use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::{iter::Peekable, ops::Range};

use arboard::Clipboard;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::buffer::Buffer;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Style};
use ratatui::text::Line;
use ratatui::widgets::{Block, Paragraph, Widget};

use crate::code::editor::ops::DeleteXOp;
use crate::code::{ChasmWidget, Metadata, WidgetContext};
use crate::project::ModulePath;
use ops::{DeleteBackOp, DeleteForwardOp, DeleteVisualOp, EditOp, InsertLineOp, InsertOp, InsertSingleOp, InsertVisualOp, SplitOp};
use undo_redo::UndoRedoStack;
use search::Search;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum EditorMode {
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

pub (super) struct Editor {
    mod_or_file: ModuleOrFile,
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

    search: Search,
    undo_redo: UndoRedoStack,
    clipboard: Result<Clipboard, arboard::Error>,

    // yanked/deleted text
    content_register: Option<String>,
}
impl std::fmt::Debug for Editor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Editor")
            .field("mod_or_file", &self.mod_or_file)
            .field("code", &"<code hidden>")
            .field("scroll_y", &self.scroll_y)
            .field("mode", &self.mode)
            .field("cursor_y", &self.cursor_y)
            .field("cursor_x", &self.cursor_x)
            .field("last_requested_x", &self.last_requested_x)
            .field("active_selection", &self.active_selection)
            .field("clipboard", &self.clipboard.as_ref().map(|_| "<clipboard>"))
            .field("undo_redo", &self.undo_redo)
            .field("content_register", &self.content_register)
            .finish()
    }
}
impl Editor {
    pub (super) fn new(mod_or_file: ModuleOrFile) -> Self {
        let path = match &mod_or_file {
            ModuleOrFile::Module(module) => &module.path,
            ModuleOrFile::File(path) => path,
        };

        let code = fs::read_to_string(&path).unwrap_or_else(|e| {
            format!("Error loading {}: {}\n\nReload or something...", path.display(), e)
        });

        let code = raw_text_to_lines(code);

        Self {
            mod_or_file,
            code,
            scroll_y: 0,
            mode: EditorMode::Normal,
            cursor_y: 0,
            cursor_x: 0,
            last_requested_x: 0,
            active_selection: None,
            search: Search::new(),
            undo_redo: UndoRedoStack::new(),
            clipboard: Clipboard::new(),
            content_register: None,
        }
    }

    #[cfg(test)]
    fn new_test() -> Self {
        Editor {
            mod_or_file: ModuleOrFile::File(PathBuf::new()),
            code: Vec::new(),
            cursor_x: 0,
            cursor_y: 0,
            last_requested_x: 0,
            scroll_y: 0,
            mode: EditorMode::Normal,
            active_selection: None,
            content_register: None,
            search: Search::new(),
            undo_redo: UndoRedoStack::new(),
            clipboard: Err(arboard::Error::ContentNotAvailable), // default to empty clipboard
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
        match self.mode {
            // we can be off the end of visible text in insert mode, but not in normal mode
            EditorMode::Insert => self.code[self.cursor_y].len(),
            EditorMode::Normal => self.code[self.cursor_y].len().saturating_sub(1),
        }
    }

    fn clamp_cursor_x(&mut self) {
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

    fn position_includes_newline(&self, pos: &Position) -> bool {
        let line = &self.code[pos.line];

        if line.is_empty() {
            true
        } else if pos.column == line.len() {
            true
        } else {
            false
        }
    }

    fn is_last_line(&self, line_num: usize) -> bool {
        line_num == self.code.len().saturating_sub(1)
    }

    fn visual_delete(&mut self, op: DeleteVisualOp, ctx: &mut WidgetContext) -> Option<EditOp> {
        if op.selection.is_empty() {
            return self.delete_forward();
        }

        let old_pos = Position { line: self.cursor_y, column: self.cursor_x };
        let (start, end) = selection_absolute_order(&op.selection);

        let content = if start.line == end.line {
            let code_line = &self.code[start.line];

            if self.position_includes_newline(&end) {
                let line = format!("{}\n", &code_line[start.column..]);
                self.code[start.line].replace_range(start.column.., "");

                if ! self.is_last_line(end.line) {
                    let removed_line = self.code.remove(end.line + 1);
                    self.code[start.line].push_str(&removed_line);
                }

                Content::from(line)
            } else {
                let content = Content::from(code_line[start.column..=end.column].to_string());
                self.code[start.line].replace_range(start.column..=end.column, "");

                content
            }
        } else {
            let mut lines = Vec::new(); 
            let mut current = start;
            let line = self.code[start.line][start.column..].to_string();
            self.code[start.line].replace_range(start.column.., "");
            let first_line = Text::Line(vec![Span::from(line)]);
            lines.push(first_line);

            current.line += 1;
            let operating_line = current.line;

            while current.line < end.line {
                let line = self.code.remove(operating_line);
                lines.push(Text::Line(vec![Span::from(line)]));
                current.line += 1;
            }

            if self.position_includes_newline(&Position { line: operating_line , column: end.column }) {
                let line = self.code.remove(operating_line);
                if operating_line < self.code.len() {
                    let line_to_append = self.code.remove(operating_line);
                    self.code[start.line].push_str(&line_to_append);
                }
                lines.push(Text::Line(vec![Span::from(line)]));
            } else {
                let line = self.code[operating_line][..=end.column].to_string();
                self.code[operating_line].replace_range(..=end.column, "");
                let line_to_append = self.code.remove(operating_line);
                self.code[start.line].push_str(&line_to_append);
                lines.push(Text::Span(Span::from(line)));
            }

            Content::Lines(lines)
        };

        let target_pos = Position { line: start.line, column: start.column };
        self.move_to(target_pos, &ctx.metadata);

        let inverse_op = InsertVisualOp {
            selection: op.selection,
            content,
            cursor_to: Position { line: self.cursor_y, column: self.cursor_x },
            cursor_from: old_pos,
        };

        Some(inverse_op.into())
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

    fn get_search_pos_after_cursor(&self) -> Position {
        let mut pos = Position { line: self.cursor_y, column: self.cursor_x };

        if pos.column < self.code[pos.line].len() {
            pos.column += 1;
        } else if pos.line < self.code.len() - 1 {
            pos.line += 1;
            pos.column = 0;
        } else {
            pos.line = 0;
            pos.column = 0;
        }

        pos
    }

    fn paste_from_clipboard(&mut self, ctx: &mut WidgetContext) {
        let Ok(clipboard) = self.clipboard.as_mut() else {
            let cberr = self.clipboard.as_ref().err().unwrap();
            ctx.log(format!("Can't access clipboard for paste: {cberr}"));
            return;
        };
        
        let content = match clipboard.get_text() {
            Ok(text) => text,
            Err(e) => {
                ctx.log(format!("Failed to get clipboard text for paste: {e}"));
                return;
            }
        };

        if content.is_empty() {
            return;
        }

        let editor_content = Content::from(content);
        let pseudo_selection = self.get_selection_from(&editor_content);
        let (sel_start, sel_end) = selection_absolute_order(&pseudo_selection);

        let insert_op = InsertVisualOp {
            selection: pseudo_selection,
            content: editor_content,
            cursor_from: sel_start,
            cursor_to: sel_end,
        };

        if let Some(inverse_op) = self.apply(insert_op.into(), ctx) {
            self.undo_redo.push(inverse_op);
        }

    }

    fn copy_selection_to_register(&mut self, selection: &VisualSelection, _ctx: &mut WidgetContext) {
        // TODO: this converts the selection to String content. We'd rather have the original `Content` type,
        // but creating it is complected in `fn visual_delete` so we're just using this for now
        let content = self.get_selection_content(selection);

        if content.is_empty() {
            return;
        }

        self.content_register = Some(content);
    }

    fn copy_selection_to_clipboard(&mut self, selection: &VisualSelection, ctx: &mut WidgetContext) {
        let selection_content = self.get_selection_content(selection);

        if selection_content.is_empty() {
            return;
        }

        self.set_clipboard_text(selection_content, ctx);
    }

    fn get_selection_content(&self, selection: &VisualSelection) -> String {
        let (start, end) = selection_absolute_order(selection);
        let content = if start.line == end.line {
            // all content on the same line
            let line = &self.code[start.line];
            if line.is_empty() {
                String::from("\n")
            } else if end.column == line.len() {
                // user selected the logical newline (like vim '$')
                let mut content = line[start.column..end.column].to_string();
                content.push('\n');

                content
            } else {
                // visual select is an inclusive range
                line[start.column..=end.column].to_string()
            }
        } else {
            let mut lines = Vec::with_capacity(end.line - start.line);
            lines.push(&self.code[start.line][start.column..]);

            for line in (start.line + 1)..end.line {
                lines.push(self.code[line].as_str());
            }

            let end_line_range = if self.code[end.line].is_empty() {
                0..0
            } else if end.column == self.code[end.line].len() {
                // user selected the logical newline (like vim '$')
                0..end.column
            } else {
                // visual select is an inclusive range
                0..end.column + 1
            };

            lines.push(&self.code[end.line][end_line_range]);
            let mut joined_lines = lines.join("\n");

            let end_line_len = self.code[end.line].len();
            if end_line_len == 0 || end.column == end_line_len {
                // user selected the logical newline (like vim '$')
                joined_lines.push('\n');
            }

            joined_lines
        };

        content
    }

    fn set_clipboard_text(&mut self, text: String, ctx: &mut WidgetContext) {
        let Ok(clipboard) = self.clipboard.as_mut() else {
            let cberr = self.clipboard.as_ref().err().unwrap();
            ctx.log(format!("Can't access clipboard for cut/copy: {cberr}"));
            return;
        };

        if let Err(e) = clipboard.set_text(text) {
            ctx.log(format!("Failed to set clipboard text during cut/copy: {e}"));
        }
    }

    fn save_buffer(&self, ctx: &mut WidgetContext) {
        let path = self.mod_or_file.path();
        let file = match File::create(&path) {
            Ok(f) => f,
            Err(e) => {
                ctx.log(format!("Failed to save file {}: {e}", path.display()));
                return;
            }
        };

        let mut writer = BufWriter::new(file);

        let nlines = self.code.len();
        for (i, line) in self.code.iter().enumerate() {
            if let Err(e) = writer.write_all(line.as_bytes()) {
                ctx.log(format!("Failed to write to file {}: {e}", path.display()));
                return;
            }

            if i != nlines - 1 {
                if let Err(e) = writer.write_all(b"\n") {
                    ctx.log(format!("Failed to write to file {}: {e}", path.display()));
                    return;
                }
            }
        }
    }

    /// Applies the edit operation and returns the inverse *if* something was performed.
    fn apply(&mut self, op: EditOp, ctx: &mut WidgetContext) -> Option<EditOp> {
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

                self.move_to(insert_op.cursor_to, &ctx.metadata);
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

                self.move_to(delete_op.cursor_to, &ctx.metadata);
                Some(inverse_op.into())

            }
            EditOp::InsertSingle(insert_single_op) => {
                let inverse_op = insert_single_op.clone().invert();

                let current_line = &mut self.code[insert_single_op.at.line];
                let col = insert_single_op.at.column;

                let c = insert_single_op.content;
                current_line.insert(col, c);

                self.move_to(insert_single_op.cursor_to, &ctx.metadata);
                Some(inverse_op.into())
            }
            EditOp::InsertVisual(insert_visual_op) => {
                let inverse_op = insert_visual_op.clone().invert();

                let (start, _end) = selection_absolute_order(&insert_visual_op.selection);
                let current_line = &mut self.code[start.line];
                let col = start.column;

                match insert_visual_op.content {
                    Content::Lines(lines) => {
                        let last_line = current_line.split_off(col);
                        let mut lines = lines.into_iter();

                        match lines.next().unwrap() {
                            Text::Line(spans) => {
                                for span in spans {
                                    current_line.push_str(&span.content);
                                }
                            }
                            Text::Span(span) => {
                                panic!("Unexpected Text::Span in InsertVisual content (the first line) where Text::Line was expected: {span:#?}");
                            }
                        }

                        let mut rev_lines = lines.rev();
                        if let Some(last_insert_line) = rev_lines.next() {

                            match last_insert_line {
                                Text::Line(spans) => {
                                    let insert_content: String = spans.into_iter().map(|span| span.content).collect();
                                    self.code.insert(start.line + 1, last_line);
                                    self.code.insert(start.line + 1, insert_content);
                                }
                                Text::Span(span) => {
                                    let last_line = format!("{}{}", span.content, last_line);
                                    self.code.insert(start.line + 1, last_line);
                                }
                            }

                            for line in rev_lines {
                                match line {
                                    Text::Line(spans) => {
                                        let insert_content: String = spans.into_iter().map(|span| span.content).collect();
                                        self.code.insert(start.line + 1, insert_content);
                                    }
                                    Text::Span(span) => {
                                        panic!("Unexpected Text::Span in InsertVisual content (the middle splice) where Text::Line was expected: {span:#?}");
                                    }
                                }
                            }
                        } else {
                            self.code.insert(start.line + 1, last_line);
                        }
                    }
                    Content::StringOrChar(StringOrChar::String(s)) => {
                        current_line.insert_str(col, &s);
                    }
                    Content::StringOrChar(StringOrChar::Char(c)) => {
                        current_line.insert(col, c);
                    }
                }
                
                self.move_to(insert_visual_op.cursor_to, &ctx.metadata);
                Some(inverse_op.into())
            }
            EditOp::InsertLine(insert_line_op) => {
                let inverse_op = insert_line_op.clone().invert();

                self.code.insert(insert_line_op.y_pos, insert_line_op.content);
                self.move_to(insert_line_op.cursor_to, &ctx.metadata);
                Some(inverse_op.into())
            }
            EditOp::DeleteLine(delete_line_op) => {
                let inverse_op = delete_line_op.clone().invert();

                self.code.remove(delete_line_op.y_pos);
                self.move_to(delete_line_op.cursor_to, &ctx.metadata);
                Some(inverse_op.into())
            }
            EditOp::Split(split_op) => {
                let line = &mut self.code[split_op.at.line];
                let new_line = line.split_off(split_op.at.column);
                self.code.insert(split_op.at.line + 1, new_line);
                self.move_to(split_op.cursor_to, &ctx.metadata);

                let inverse = split_op.invert();
                Some(inverse.into())
            }
            EditOp::Join(join_op) => {
                let current_line = self.code.remove(join_op.at.line + 1);
                let line = &mut self.code[join_op.at.line];
                line.push_str(&current_line);
                self.move_to(join_op.cursor_to, &ctx.metadata);

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
                let inverse_op = self.visual_delete(delete_visual_op, ctx);

                // Currently depend on this being true for 'cut' operation (delete + copy to clipboard)
                assert!(matches!(inverse_op, Some(EditOp::InsertVisual(_)) | Some(EditOp::InsertSingle(_)) | Some(EditOp::Split(_)) | None));
                inverse_op
            },
        }
    }

    fn handle_normal_mode_event(&mut self, event: &Event, ctx: &mut WidgetContext) {
        if ! event.is_key_press() {
            return;
        }

        let Event::Key(key_event) = event else { unreachable!() };

        if self.search.is_active() {
            self.handle_normal_mode_search_input(key_event, ctx);
        } else {
            self.handle_normal_mode_input(key_event, ctx);
        }

        let new_cursor_pos = Position { line: self.cursor_y, column: self.cursor_x };

        if self.should_update_visual_selection() {
            let selection = self.active_selection.as_mut().unwrap();
            selection.cursor = new_cursor_pos;
        }
    }

    fn handle_normal_mode_search_input(&mut self, key_event: &KeyEvent, ctx: &mut WidgetContext) {
        assert!(self.search.is_active());

        match key_event.code {
            KeyCode::Esc => {
                self.search.deactivate();
            }
            KeyCode::Enter => {
                self.search.store();
            }
            KeyCode::Char(c) => {
                self.search.push(c);
            }
            KeyCode::Backspace => {
                self.search.pop();
            }
            _ => { /* Nothing */ }
        }
    }

    fn handle_normal_mode_input(&mut self, key_event: &KeyEvent, ctx: &mut WidgetContext) {
        assert!(! self.search.is_active());

        match key_event.code {
            KeyCode::Esc => {
                if self.active_selection.is_some() {
                    self.active_selection = None;
                    self.clamp_cursor_x();
                }
            }
            KeyCode::Char('/') => {
                self.search.activate();
            }
            KeyCode::Char('G') => {
                let target_line = self.code.len().saturating_sub(1);
                if target_line != self.cursor_y {
                    let move_len = target_line.saturating_sub(self.cursor_y);
                    self.move_cursor_down(move_len, &ctx.metadata);
                }
            }
            KeyCode::Char('g') if key_event.modifiers == KeyModifiers::CONTROL => {
                let target_line = 0;
                if target_line != self.cursor_y {
                    let move_len = self.cursor_y.saturating_sub(target_line);
                    self.move_cursor_up(move_len, &ctx.metadata);
                }

            }
            KeyCode::Char('n') => {
                if self.code.is_empty() {
                    return;
                }

                let from_pos = self.get_search_pos_after_cursor();

                if let Some(match_pos) = self.search.next_match_fwd(from_pos, &self.code) {
                    self.move_to(match_pos, &ctx.metadata);
                }
            }
            KeyCode::Char('I') => {
                self.cursor_x = 0;
                self.skip_x_whitespace_forward();
                self.mode = EditorMode::Insert;
                self.active_selection = None;
            }
            KeyCode::Char('x') => {
                if let Some(selection) = self.active_selection.take() {
                    if !key_event.modifiers.contains(KeyModifiers::CONTROL) {
                        // overwrite the register only if we're not copying to clipboard
                        self.copy_selection_to_register(&selection, ctx);
                    }

                    let visual_delete_op = DeleteVisualOp {
                        selection,
                        deleted: None,
                    };

                    if let Some(inverse_op) = self.apply(visual_delete_op.into(), ctx) {
                        if key_event.modifiers == KeyModifiers::CONTROL {
                            let deleted_content = match &inverse_op {
                                EditOp::InsertVisual(insert_visual_op) => insert_visual_op.content.to_string(),
                                EditOp::InsertSingle(insert_single_op) => insert_single_op.content.to_string(),
                                EditOp::Split(_) => '\n'.to_string(),
                                other => panic!("Unexpected inverse op for visual delete: {other:#?}"),
                            };

                            self.set_clipboard_text(deleted_content, ctx);
                        }

                        self.undo_redo.push(inverse_op);
                    }
                } else {
                    let edit = DeleteXOp {
                        at: Position { line: self.cursor_y, column: self.cursor_x },
                        content: None,
                        cursor_to: Position { line: self.cursor_y, column: self.cursor_x },
                        cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
                    };

                    if let Some(inverse_op) = self.apply(edit.into(), ctx) {
                        self.undo_redo.push(inverse_op);
                    }
                }
            }
            KeyCode::Char('d') => {
                if key_event.modifiers == KeyModifiers::CONTROL {
                    let move_size = (ctx.metadata.view_area.0.height as usize).saturating_sub(1);
                    self.move_cursor_down(move_size, &ctx.metadata);
                } else if let Some(selection) = self.active_selection.take() {
                    self.copy_selection_to_register(&selection, ctx);

                    let visual_delete = DeleteVisualOp {
                        selection,
                        deleted: None,
                    };

                    if let Some(inverse_op) = self.apply(visual_delete.into(), ctx) {
                        self.undo_redo.push(inverse_op);
                    }
                }
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
            }
            KeyCode::Char('y') => {
                if let Some(selection) = self.active_selection.take() {
                    self.copy_selection_to_register(&selection, ctx);
                    self.move_to(selection.anchor, &ctx.metadata);
                }
            }
            KeyCode::Char('p') => {
                if self.content_register.is_none() {
                    return;
                }

                let string_content = self.content_register.as_ref().unwrap().clone(); 
                let editor_content = Content::from(string_content);

                let pseudo_selection = self.get_selection_from(&editor_content);
                let (sel_start, _sel_end) = selection_absolute_order(&pseudo_selection);

                let insert_op = InsertVisualOp {
                    selection: pseudo_selection,
                    content: editor_content,
                    cursor_from: sel_start,
                    cursor_to: sel_start, // cursor stays on 'p'
                };

                if let Some(inverse_op) = self.apply(insert_op.into(), ctx) {
                    self.undo_redo.push(inverse_op);
                }
            }
            KeyCode::Char('s') => {
                if key_event.modifiers == KeyModifiers::CONTROL {
                    // save
                   self.save_buffer(ctx); 
                } else if let Some(selection) = self.active_selection.take() {
                    // visual delete and enter insert mode

                    self.copy_selection_to_register(&selection, ctx);
                    let visual_delete = DeleteVisualOp {
                        selection,
                        deleted: None,
                    };

                    if let Some(inverse_op) = self.apply(visual_delete.into(), ctx) {
                        self.undo_redo.push(inverse_op);
                    }

                    self.mode = EditorMode::Insert;
                } else {
                    // delete the character under the cursor and enter insert mode
                    let is_empty = self.code[self.cursor_y].is_empty();

                    if !is_empty {
                        let delete_forward = DeleteForwardOp {
                            at: Position { line: self.cursor_y, column: self.cursor_x },
                            cursor_to: Position { line: self.cursor_y, column: self.cursor_x },
                            cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
                            content: None,
                        };

                        if let Some(undo_op) = self.apply(delete_forward.into(), ctx) {
                            self.undo_redo.push(undo_op);
                        }
                    }

                    self.mode = EditorMode::Insert;
                }
            }
            KeyCode::Char('O') => {
                if let Some(selection) = self.active_selection.as_mut() {
                    selection.swap_anchor_and_cursor(); 
                    self.cursor_x = selection.cursor.column;
                    self.cursor_y = selection.cursor.line;
                    self.last_requested_x = self.cursor_x;
                    self.snap_view_to_cursor(&ctx.metadata);
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

                    let inverse_op = self.apply(insert_op, ctx).unwrap();
                    self.undo_redo.push(inverse_op);

                    self.mode = EditorMode::Insert;
                }
            }
            KeyCode::Char('o') => {
                if let Some(selection) = self.active_selection.as_mut() {
                    selection.swap_anchor_and_cursor(); 
                    self.cursor_x = selection.cursor.column;
                    self.cursor_y = selection.cursor.line;
                    self.last_requested_x = self.cursor_x;
                    self.snap_view_to_cursor(&ctx.metadata);
                } else {
                    let insert_line_op = InsertLineOp {
                        y_pos: self.cursor_y + 1,
                        content: String::new(),
                        cursor_to: Position { line: self.cursor_y + 1, column: 0 },
                        cursor_from: Position { line: self.cursor_y, column: 0 },
                    };

                    let inverse_op = self.apply(insert_line_op.into(), ctx).unwrap();
                    self.undo_redo.push(inverse_op);

                    self.mode = EditorMode::Insert;
                }
            }
            KeyCode::Char('J') | KeyCode::Down if key_event.modifiers.contains(KeyModifiers::SHIFT) => {
                self.scroll_y = self.scroll_y.saturating_add(1);
            }
            KeyCode::Char('K') | KeyCode::Up if key_event.modifiers.contains(KeyModifiers::SHIFT) => {
                self.scroll_y = self.scroll_y.saturating_sub(1);
            }
            KeyCode::Char('u') => {
                if key_event.modifiers == KeyModifiers::CONTROL {
                    let move_size = (ctx.metadata.view_area.0.height as usize).saturating_sub(1);
                    self.move_cursor_up(move_size, &ctx.metadata);
                } else {
                    if let Some(op) = self.undo_redo.undo() {
                        self.apply(op, ctx);
                    }
                }

            }
            KeyCode::Char('r') => {
                if key_event.modifiers != KeyModifiers::CONTROL {
                    return;
                }

                if let Some(op) = self.undo_redo.redo() {
                    self.apply(op, ctx);
                }

            }
            KeyCode::Char('j') | KeyCode::Down => {
                let move_size = get_vertical_move_distance(key_event, &ctx.metadata);
                self.move_cursor_down(move_size, &ctx.metadata);
            }
            KeyCode::Char('k') | KeyCode::Up => {
                let move_size = get_vertical_move_distance(key_event, &ctx.metadata);
                self.move_cursor_up(move_size, &ctx.metadata);
            }
            KeyCode::Char('h') | KeyCode::Left => {
                self.move_cursor_left(1);
            }
            KeyCode::Char('l') | KeyCode::Right => {
                self.move_cursor_right(1);
            }
            KeyCode::Char('0') => {
                self.cursor_x = 0;
                self.last_requested_x = self.cursor_x;
            }
            KeyCode::Char('$') => {
                let max_cursor_x = self.max_cursor_x();
                self.cursor_x = max_cursor_x;

                if self.active_selection.is_some() && ! self.code[self.cursor_y].is_empty()  {
                    // vim 'v$' includes the logical newline if we're not on an empty line
                    self.cursor_x += 1;
                }

                self.last_requested_x = self.cursor_x;
            }
            KeyCode::Char('w') => {
                self.move_to_next_word(&ctx.metadata);
            }
            KeyCode::Char('e') => {
                self.move_to_next_word_end(&ctx.metadata);
            }
            KeyCode::Char('b') => {
                self.move_to_previous_word_start(&ctx.metadata);
            }
            KeyCode::Char('i') => {
                if self.active_selection.is_none() {
                    self.mode = EditorMode::Insert;
                }
            }
            KeyCode::Char('v') => {
                if key_event.modifiers.is_empty() {
                    self.toggle_selection();
                } else if key_event.modifiers == KeyModifiers::CONTROL {
                    if self.active_selection.is_none() {
                        self.paste_from_clipboard(ctx);
                    }
                }
            }
            KeyCode::Char('c') => {
                if key_event.modifiers == KeyModifiers::CONTROL && let Some(selection) = self.active_selection.take() {
                    self.copy_selection_to_clipboard(&selection, ctx);
                    self.cursor_x = self.cursor_x.min(self.max_cursor_x());
                    self.last_requested_x = self.cursor_x;
                } 
            }
            _other => {
                // currently unbound
                ctx.log(format!("Unbound key in normal mode: {key_event:#?}"));
            }
        };
    }

    fn handle_insert_mode_event(&mut self, event: &Event, ctx: &mut WidgetContext) {
        if ! event.is_key_press() {
            return;
        }

        let Event::Key(key_event) = event else { unreachable!() };
        match key_event.code {
            KeyCode::Esc => {
                self.mode = EditorMode::Normal;
                self.clamp_cursor_x();
            }
            KeyCode::Char('j') | KeyCode::Char('k') | KeyCode::Char('h') | KeyCode::Char('l') if key_event.modifiers == KeyModifiers::CONTROL => {
                match key_event.code {
                    KeyCode::Char('j') => {
                        self.move_cursor_down(1, &ctx.metadata);
                    }
                    KeyCode::Char('k') => {
                        self.move_cursor_up(1, &ctx.metadata);
                    }
                    KeyCode::Char('h') => {
                        self.move_cursor_left(1);
                    }
                    KeyCode::Char('l') => {
                        self.move_cursor_right(1);
                    }
                    _ => unreachable!(),
                }
            }
            KeyCode::Left => {
                self.move_cursor_left(1);
            }
            KeyCode::Right => {
                self.move_cursor_right(1);
            }
            KeyCode::Up => {
                self.move_cursor_up(1, &ctx.metadata);
            }
            KeyCode::Down => {
                self.move_cursor_down(1, &ctx.metadata);
            }
            KeyCode::Char('v') if key_event.modifiers == KeyModifiers::CONTROL => {
                self.paste_from_clipboard(ctx);
            }
            KeyCode::Char(c) => {
                let insert_op = InsertSingleOp {
                    at: Position { line: self.cursor_y, column: self.cursor_x },
                    content: c.into(),
                    cursor_to: Position { line: self.cursor_y, column: self.cursor_x + 1 },
                    cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
                };

                let undo_op = self.apply(insert_op.into(), ctx).unwrap();
                self.undo_redo.push(undo_op);
            }
            KeyCode::Backspace => {
                let delete_back = DeleteBackOp {
                    at: Position { line: self.cursor_y, column: self.cursor_x },
                    cursor_to: Position { line: self.cursor_y, column: self.cursor_x.saturating_sub(1) },
                    cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
                    content: None,
                };
                if let Some(undo_op) = self.apply(delete_back.into(), ctx) {
                    self.undo_redo.push(undo_op);
                }
            }
            KeyCode::Delete => {
                let delete_forward = DeleteForwardOp {
                    at: Position { line: self.cursor_y, column: self.cursor_x },
                    cursor_to: Position { line: self.cursor_y, column: self.cursor_x },
                    cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
                    content: None,
                };

                if let Some(undo_op) = self.apply(delete_forward.into(), ctx) {
                    self.undo_redo.push(undo_op);
                }
            }
            KeyCode::Enter => {
                let split_op = SplitOp {
                    at: Position { line: self.cursor_y, column: self.cursor_x },
                    cursor_to: Position { line: self.cursor_y + 1, column: 0 },
                    cursor_from: Position { line: self.cursor_y, column: self.cursor_x },
                };

                let undo_op = self.apply(split_op.into(), ctx).unwrap();
                self.undo_redo.push(undo_op);
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

                let undo_op = self.apply(insert_op.into(), ctx).unwrap();
                self.undo_redo.push(undo_op);
            }
            _other => {
                ctx.log(format!("Unbound key in insert mode: {key_event:#?}"));
            }
        }
    }
    
    fn get_selection_from(&self, editor_content: &Content) -> VisualSelection {
        let cursor = Position { line: self.cursor_y, column: self.cursor_x };

        match editor_content {
            Content::StringOrChar(string_or_char) => {
                match string_or_char {
                    StringOrChar::String(s) => {
                        let to = Position { column: (cursor.column + s.len()).saturating_sub(1), ..cursor };
                        VisualSelection { anchor: cursor, cursor: to }
                    }
                    StringOrChar::Char(_) => {
                        VisualSelection { anchor: cursor, cursor }
                    }
                }
            }
            Content::Lines(lines) => {
                let line_count = lines.iter().filter(|line| matches!(line, Text::Line(_))).count() - 1;
                let last_line_len = match lines.last().unwrap() {
                    Text::Span(s) => s.content.len().saturating_sub(1),
                    Text::Line(spans) => spans.iter().map(|span| span.content.len()).sum(), // the full line + implied newline
                };

                let end_line = cursor.line + line_count;
                let end_col = last_line_len;
                VisualSelection { anchor: cursor, cursor: Position { line: end_line, column: end_col } }
            }
        }
    }

}
impl ChasmWidget for Editor {
    
    fn handle_event(&mut self, event: &Event, ctx: &mut WidgetContext) {
        match self.mode {
            EditorMode::Normal => self.handle_normal_mode_event(event, ctx),
            EditorMode::Insert => self.handle_insert_mode_event(event, ctx),
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

fn get_vertical_move_distance(kev: &KeyEvent, meta: &Metadata) -> usize {
    if kev.modifiers == KeyModifiers::CONTROL {
        (meta.view_area.0.height as usize).saturating_sub(1)
    } else {
        1
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

/// Converts a "normal" string (e.g. from a file or clipboard) into a collection of lines.
/// 
/// Carriage returns are stripped iff they're part of the newline (\r\n), and final newline is
/// preserved if it exists (unlike str::lines()).
fn raw_text_to_lines(text: String) -> Vec<String> {
    text.split('\n')
        .into_iter()
        .map(|s| s.strip_suffix('\r').unwrap_or(s))
        .map(String::from)
        .collect()
}

#[cfg(test)]
mod tests {
    use crossterm::event::{KeyEventKind, KeyEventState};

    use crate::code::AppContext;

    use super::*;

    fn keypress_event(c: char, modifiers: KeyModifiers) -> Event {
        Event::Key(KeyEvent {
            code: KeyCode::Char(c),
            modifiers,
            kind: KeyEventKind::Press,
            state: KeyEventState::NONE
        })
    }

    fn apply_sequence(seq: &str, editor: &mut Editor, ctx: &mut WidgetContext) {
        for c in seq.chars() {
            let event = keypress_event(c, KeyModifiers::empty());
            editor.handle_event(&event, ctx);
        }
    }

    #[test]
    fn editor_text_to_lines_includes_preceding_and_trailing_newlines() {
        let text = "\r\nline1\r\nline2\r\nline3\r\n".to_string();

        let lines = raw_text_to_lines(text);

        assert_eq!(lines, vec!["", "line1", "line2", "line3", ""]);
    }

    #[test]
    fn editor_v_sets_to_visual_mode() {
        let mut editor = Editor::new_test();
        let mut app_ctx = AppContext::new_test();
        let exp_selection = VisualSelection {
            anchor: Position { line: 0, column: 0},
            cursor: Position { line: 0, column: 0},
        };

        apply_sequence("v", &mut editor, &mut app_ctx.widget_context());

        assert!(editor.active_selection.is_some());
        assert_eq!(editor.active_selection.unwrap(), exp_selection);
    }

    #[test]
    fn editor_visual_delete_on_empty_line_removes_first_newline() {
        let mut editor = Editor::new_test();
        let code = vec!["".to_string(), "line1".to_string(), "line2".to_string()];
        let exp_code = vec!["line1".to_string(), "line2".to_string()];
        editor.code = code;
        editor.cursor_x = 0;
        editor.cursor_y = 0;
        let mut app_ctx = AppContext::new_test();

        apply_sequence("vd", &mut editor, &mut app_ctx.widget_context());

        assert_eq!(editor.code, exp_code);
    }

    #[test]
    fn editor_visual_delete_entire_line_removes_line() {
        let mut editor = Editor::new_test();
        let code = vec!["line1".to_string(), "line2".to_string()];
        let exp_code = vec!["line2".to_string()];
        editor.code = code;
        let mut app_ctx = AppContext::new_test();

        apply_sequence("v$d", &mut editor, &mut app_ctx.widget_context());

        assert_eq!(editor.code, exp_code);
    }

    #[test]
    fn editor_visual_delete_range_removes_range() {
        let mut editor = Editor::new_test();
        let code = vec!["line1".to_string()];
        let exp_code = vec!["l1".to_string()];
        editor.code = code;
        let mut app_ctx = AppContext::new_test();

        apply_sequence("lvlld", &mut editor, &mut app_ctx.widget_context());

        assert_eq!(editor.code, exp_code);
    }
 
    #[test]
    fn editor_visual_delete_including_newline_removes_newline() {
        let mut editor = Editor::new_test();
        let code = vec!["line1".to_string(), "line2".to_string()];
        let exp_code = vec!["linline2".to_string()];
        editor.code = code;
        let mut app_ctx = AppContext::new_test();

        apply_sequence("lllv$d", &mut editor, &mut app_ctx.widget_context());

        assert_eq!(editor.code, exp_code);
    } 
 
    #[test]
    fn editor_visual_delete_to_next_line_concats_lines() {
        let mut editor = Editor::new_test();
        let code = vec!["line1".to_string(), "line2".to_string()];
        let exp_code = vec!["linne2".to_string()];
        editor.code = code;
        let mut app_ctx = AppContext::new_test();

        apply_sequence("lllv0jld", &mut editor, &mut app_ctx.widget_context());

        assert_eq!(editor.code, exp_code);
    } 
 
    #[test]
    fn editor_visual_delete_including_next_line_newline_deletes_last_line() {
        let mut editor = Editor::new_test();
        let code = vec!["line1".to_string(), "line2".to_string(), "line3".to_string()];
        let exp_code = vec!["liline3".to_string()];
        editor.code = code;
        let mut app_ctx = AppContext::new_test();

        apply_sequence("llvj$d", &mut editor, &mut app_ctx.widget_context());

        assert_eq!(editor.code, exp_code);
    } 
 
    #[test]
    fn editor_visual_yank_works_on_single_line_ranges() {
        let mut editor = Editor::new_test();
        let code = vec!["line1".to_string()];
        editor.code = code;
        let mut app_ctx = AppContext::new_test();

        apply_sequence("lvlly", &mut editor, &mut app_ctx.widget_context());

        let string_content = editor.content_register.as_ref().unwrap();
        assert_eq!(string_content, "ine");
    } 
 
    #[test]
    fn editor_visual_yank_works_on_empty_ranges() {
        let mut editor = Editor::new_test();
        let code = vec!["line1".to_string()];
        editor.code = code;
        let mut app_ctx = AppContext::new_test();

        apply_sequence("vy", &mut editor, &mut app_ctx.widget_context());

        let string_content = editor.content_register.as_ref().unwrap();
        assert_eq!(string_content, "l");
    } 
 
    #[test]
    fn editor_visual_yank_works_on_multi_line_ranges_ending_with_empty_newline() {
        let mut editor = Editor::new_test();
        let code = vec![
            "line1".to_string(),
            "line2".to_string(),
            "".to_string(),
            "line3".to_string()
        ];
        editor.code = code;
        let mut app_ctx = AppContext::new_test();

        apply_sequence("lvjjy", &mut editor, &mut app_ctx.widget_context());

        let string_content = editor.content_register.as_ref().unwrap();
        assert_eq!(string_content, "ine1\nline2\n\n");
    }
  
    #[test]
    fn editor_visual_yank_works_on_multi_line_ranges_ending_with_newline() {
        let mut editor = Editor::new_test();
        let code = vec![
            "line1".to_string(),
            "line2".to_string(),
            "line3".to_string()
        ];
        editor.code = code;
        let mut app_ctx = AppContext::new_test();

        apply_sequence("vj$y", &mut editor, &mut app_ctx.widget_context());

        let string_content = editor.content_register.as_ref().unwrap();
        assert_eq!(string_content, "line1\nline2\n");
    }

    #[test]
    fn editor_visual_delete_with_s_yanks_and_puts_in_insert_mode() {
        let mut editor = Editor::new_test();
        let code = vec!["line1".to_string()];
        let exp_code = vec!["l1".to_string()];
        editor.code = code;
        let mut app_ctx = AppContext::new_test();

        apply_sequence("lvlls", &mut editor, &mut app_ctx.widget_context());

        assert!(editor.content_register.is_some(), "Expected register content after 's' command");
        let string_content = editor.content_register.as_ref().unwrap();
        assert_eq!(string_content, "ine");
        assert_eq!(editor.mode, EditorMode::Insert);
        assert_eq!(editor.code, exp_code);
        assert_eq!(editor.cursor_x, 1);
        assert_eq!(editor.cursor_y, 0);
    }

    #[test]
    fn editor_visual_delete_with_x_yanks_and_deletes() {
        let mut editor = Editor::new_test();
        let code = vec![
            "line1".to_string(),
            "line2".to_string(),
        ];
        let exp_code = vec!["2".to_string()];
        editor.code = code;
        let mut app_ctx = AppContext::new_test();

        apply_sequence("vj$hhx", &mut editor, &mut app_ctx.widget_context());

        assert!(editor.content_register.is_some(), "Expected register content after 'x' command");
        let string_content = editor.content_register.as_ref().unwrap();
        assert_eq!(string_content, "line1\nline");
        assert_eq!(editor.code, exp_code);
        assert_eq!(editor.cursor_x, 0);
        assert_eq!(editor.cursor_y, 0);
    }

    #[test]
    fn editor_visual_delete_with_d_yanks_and_deletes() {
        let mut editor = Editor::new_test();
        let code = vec![
            "line1".to_string(),
            "line2".to_string(),
            "line3".to_string(),
        ];
        let exp_code = vec![
            "line1".to_string(),
            "line3".to_string(),
        ];
        editor.code = code;
        let mut app_ctx = AppContext::new_test();

        apply_sequence("jv$d", &mut editor, &mut app_ctx.widget_context());

        assert!(editor.content_register.is_some(), "Expected register content after 'd' command");
        let string_content = editor.content_register.as_ref().unwrap();
        assert_eq!(string_content, "line2\n");
        assert_eq!(editor.code, exp_code);
        assert_eq!(editor.cursor_x, 0);
        assert_eq!(editor.cursor_y, 1);
    }
}