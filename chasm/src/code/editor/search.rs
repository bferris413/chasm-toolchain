//! Buffer search functionality.
//! 
//! Search direction, forward or backward, is determined by which "mode"
//! the search was activated in.

use std::mem;

use crate::code::editor::Position;

pub (super) struct SearchBuffer {
    /// The search mode that is currently being input by the user, if any.
    active_mode: Option<SearchMode>,
    input: String,
    /// The last search input and mode that was stored.
    last_search: LastSearch,
}

impl SearchBuffer {
    pub fn new() -> Self {
        Self {
            active_mode: None,
            input: String::new(),
            last_search: LastSearch {
                mode: SearchMode::Forward,
                input: String::new(),
            },
        }
    }

    pub fn active_mode(&self) -> Option<SearchMode> {
        self.active_mode
    }

    pub fn last_mode(&self) -> SearchMode {
        self.last_search.mode
    }

    pub fn activate(&mut self, mode: SearchMode) {
        self.active_mode = Some(mode);
    }

    pub fn deactivate(&mut self) {
        self.active_mode = None;
        self.input.clear();
    }

    pub fn push(&mut self, c: char) {
        self.input.push(c);
    }

    pub fn store(&mut self) {
        if ! self.input.is_empty() {
            mem::swap(&mut self.input, &mut self.last_search.input);
            self.last_search.mode = self.active_mode.expect("Search is not active");
        }

        self.deactivate();
    }
    
    pub fn pop(&mut self) -> Option<char> {
        self.input.pop()
    }
    /// Returns the current search input.
    /// 
    /// Panics if search is not active.
    pub fn current_input<'a>(&'a self) -> SearchInput<'a> {
        let search_mode = self.active_mode.expect("Search is not active");

        SearchInput { 
            input: &self.input,
            search_mode,
        }
    }

    /// Search "forward" from the given position based on the last searched mode.
    pub fn next_match_fwd(&self, from_pos: Position, code: &[String]) -> Option<Position> {
        match self.last_search.mode {
            SearchMode::Forward => self.search_fwd(from_pos, code),
            SearchMode::Backward => self.search_back(from_pos, code),
        }
    }

    /// Search "backward" from the given position based on the last searched mode.
    pub fn next_match_back(&self, from_pos: Position, code: &[String]) -> Option<Position> {
        match self.last_search.mode {
            SearchMode::Forward => self.search_back(from_pos, code),
            SearchMode::Backward => self.search_fwd(from_pos, code),
        }

    }

    fn search_fwd(&self, from_pos: Position, code: &[String]) -> Option<Position> {
        if self.last_search.input.is_empty() {
            return None;
        }

        for line_no in from_pos.line..code.len() {
            let line = &code[line_no];
            if line.is_empty() {
                continue;
            }

            let search_start_col = if line_no == from_pos.line {
                from_pos.column
            } else {
                0
            };

            if let Some(col) = line[search_start_col..].find(&self.last_search.input) {
                return Some(Position { line: line_no, column: search_start_col + col });
            }
        }

        // didn't find it but we'll wrap around and try up to the starting position
        for line_no in 0..=from_pos.line {
            let line = &code[line_no];
            if line.is_empty() {
                continue;
            }

            let search_end_col = if line_no == from_pos.line {
                from_pos.column
            } else {
                line.len()
            };

            if let Some(col) = line[..search_end_col].find(&self.last_search.input) {
                return Some(Position { line: line_no, column: col });
            }
        }

        None
    }

    fn search_back(&self, from_pos: Position, code: &[String]) -> Option<Position> {
        if self.last_search.input.is_empty() {
            return None;
        }

        for line_no in (0..=from_pos.line).rev() {
            let line = &code[line_no];
            if line.is_empty() {
                continue;
            }

            let search_end_col = if line_no == from_pos.line {
                from_pos.column
            } else {
                line.len()
            };

            if let Some(col) = line[..search_end_col].rfind(&self.last_search.input) {
                return Some(Position { line: line_no, column: col });
            }
        }

        // didn't find it but we'll wrap around and try up to the starting position
        for line_no in (from_pos.line..code.len()).rev() {
            let line = &code[line_no];
            if line.is_empty() {
                continue;
            }

            let search_start_col = if line_no == from_pos.line {
                from_pos.column
            } else {
                0
            };

            if let Some(col) = line[search_start_col..].rfind(&self.last_search.input) {
                return Some(Position { line: line_no, column: search_start_col + col });
            }
        }

        None
    }
}

/// The mode of search, either forward or backward (Vim '/' vs. '?').
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub (super) enum SearchMode {
    Forward,
    Backward,
}
impl SearchMode {
    pub fn as_str(&self) -> &'static str {
        match self {
            SearchMode::Forward => "/",
            SearchMode::Backward => "?",
        }
    }
}

pub (super) struct SearchInput<'a> {
    search_mode: SearchMode,
    input: &'a str,
}
impl SearchInput<'_> {
    pub fn user_input(&self) -> &str {
        self.input
    }
}
impl std::fmt::Display for SearchInput<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.search_mode.as_str(), self.input)
    }
}

struct LastSearch {
    mode: SearchMode,
    input: String,
}
