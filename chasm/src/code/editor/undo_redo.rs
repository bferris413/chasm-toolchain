use std::collections::VecDeque;

use crate::code::editor::ops::EditOp;

#[derive(Debug)]
pub struct UndoRedoStack {
    undo: VecDeque<EditOp>,
    redo: Vec<EditOp>,
    cap: u16,
}
impl UndoRedoStack {
    pub fn new() -> Self {
        Self { undo: VecDeque::new(), redo: Vec::new(), cap: 200 }
    }

    pub fn undo(&mut self) -> Option<EditOp> {
        let op = self.undo.pop_back();

        if let Some(ref op) = op {
            self.redo.push(op.clone().invert());
        }

        op
    }

    pub fn redo(&mut self) -> Option<EditOp> {
        let op = self.redo.pop();

        if let Some(ref op) = op {
            if self.undo.len() == self.cap as usize {
                self.undo.pop_front();
            }

            self.undo.push_back(op.clone().invert());
        }

        op
    }

    pub fn push(&mut self, op: EditOp) {
        if self.undo.len() == self.cap as usize {
            self.undo.pop_front();
        }
        self.undo.push_back(op);
        self.redo.clear();
    }
}