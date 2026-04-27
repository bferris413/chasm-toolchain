//! Buffer search functionality.

use std::mem;

use crate::code::editor::Position;

pub (super) struct Search {
    is_active: bool,
    input: String,
    last_search_term: String,
}

impl Search {
    pub fn new() -> Self {
        Self {
            is_active: false,
            input: String::new(),
            last_search_term: String::new(),
        }
    }

    pub fn is_active(&self) -> bool {
        self.is_active
    }

    pub fn activate(&mut self) {
        self.is_active = true;
    }

    pub fn deactivate(&mut self) {
        self.is_active = false;
    }

    pub fn push(&mut self, c: char) {
        self.input.push(c);
    }

    pub fn store(&mut self) {
        if ! self.input.is_empty() {
            mem::swap(&mut self.input, &mut self.last_search_term);
            self.input.clear();
        }

        self.deactivate();
    }
    
    pub fn pop(&mut self) -> Option<char> {
        self.input.pop()
    }
    pub fn input<'a>(&'a self) -> SearchInput<'a> {
        SearchInput { 
            prefix: "/",
            input: &self.input,
        }
    }

    pub fn next_match_fwd(&self, from_pos: Position, code: &[String]) -> Option<Position> {
        if self.last_search_term.is_empty() {
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

            if let Some(col) = line[search_start_col..].find(&self.last_search_term) {
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

            if let Some(col) = line[..search_end_col].find(&self.last_search_term) {
                return Some(Position { line: line_no, column: col });
            }
        }

        None
    }
}

pub (super) struct SearchInput<'a> {
    prefix: &'static str,
    input: &'a str,
}
impl SearchInput<'_> {
    pub fn user_input(&self) -> &str {
        self.input
    }
}
impl std::fmt::Display for SearchInput<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.prefix, self.input)
    }
}
