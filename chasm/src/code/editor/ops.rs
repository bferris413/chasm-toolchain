use crate::code::editor::{Content, Position, StringOrChar, VisualSelection};

#[derive(Clone, Debug)]
pub struct InsertOp {
    pub at: Position,
    pub content: StringOrChar,
    pub cursor_to: Position,
    pub cursor_from: Position,
}
impl InsertOp {
    pub fn invert(self) -> DeleteOp {
        DeleteOp {
            cursor_to: self.cursor_from,
            cursor_from: self.cursor_to,
            content: Some(self.content),
            at: self.at,
        }
    }
}

#[derive(Clone, Debug)]
pub struct DeleteOp {
    pub cursor_to: Position,
    pub cursor_from: Position,
    pub content: Option<StringOrChar>,
    pub at: Position,
}
impl DeleteOp {
    pub fn invert(self) -> InsertOp {
        InsertOp {
            at: self.at,
            content: self.content.unwrap(),
            cursor_to: self.cursor_from,
            cursor_from: self.cursor_to,
        }
    }
}

#[derive(Clone, Debug)]
pub struct InsertSingleOp {
    pub at: Position,
    pub content: char,
    pub cursor_to: Position,
    pub cursor_from: Position,
}
impl InsertSingleOp {
    pub fn invert(self) -> DeleteBackOp {
        DeleteBackOp {
            cursor_to: self.cursor_from,
            cursor_from: self.cursor_to,
            content: Some(self.content),
            at: self.at,
        }
    }
}

#[derive(Clone, Debug)]
pub struct InsertVisualOp {
    pub selection: VisualSelection,
    pub content: Content,
    pub cursor_to: Position,
    #[allow(unused)]
    pub cursor_from: Position,
}
impl InsertVisualOp {
    pub fn invert(self) -> DeleteVisualOp {
        DeleteVisualOp {
            selection: self.selection,
            deleted: Some(self.content),
        }
    }
}

#[derive(Clone, Debug)]
pub struct DeleteBackOp {
    pub cursor_to: Position,
    pub cursor_from: Position,
    pub content: Option<char>,
    pub at: Position,
}
impl DeleteBackOp {
    /// Currently panics if self.content.is_none()
    pub fn invert(self) -> InsertSingleOp {
        InsertSingleOp {
            at: self.at,
            content: self.content.unwrap(),
            cursor_to: self.cursor_from,
            cursor_from: self.cursor_to,
        }
    }
}

#[derive(Clone, Debug)]
pub struct DeleteForwardOp {
    pub cursor_to: Position,
    pub cursor_from: Position,
    pub content: Option<char>,
    pub at: Position,
}
impl DeleteForwardOp {
    pub fn invert(self) -> InsertSingleOp {
        InsertSingleOp {
            at: self.at,
            content: self.content.unwrap(),
            cursor_to: self.cursor_from,
            cursor_from: self.cursor_to,
        }
    }
}

#[derive(Clone, Debug)]
pub struct DeleteXOp {
    pub cursor_to: Position,
    pub cursor_from: Position,
    pub content: Option<char>,
    pub at: Position,
}
impl DeleteXOp {
    pub fn invert(self) -> InsertSingleOp {
        InsertSingleOp {
            at: self.at,
            content: self.content.unwrap(),
            cursor_to: self.cursor_from,
            cursor_from: self.cursor_to,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SplitOp {
    pub at: Position,
    pub cursor_to: Position,
    pub cursor_from: Position,
}
impl SplitOp {
    pub fn invert(self) -> JoinOp {
        JoinOp {
            at: self.at,
            cursor_to: self.cursor_from,
            cursor_from: self.cursor_to,
        }
    }
}

#[derive(Clone, Debug)]
pub struct JoinOp {
    pub at: Position,
    pub cursor_to: Position,
    pub cursor_from: Position,
}
impl JoinOp {
    pub fn invert(self) -> SplitOp {
        SplitOp {
            at: self.at,
            cursor_to: self.cursor_from,
            cursor_from: self.cursor_to,
        }
    }
}

#[derive(Clone, Debug)]
pub struct InsertLineOp {
    pub y_pos: usize,
    pub content: String,
    pub cursor_to: Position,
    pub cursor_from: Position,
}
impl InsertLineOp {
    pub fn invert(self) -> DeleteLineOp {
        DeleteLineOp {
            y_pos: self.y_pos,
            content: self.content,
            cursor_to: self.cursor_to,
            cursor_from: self.cursor_from,
        }
    }
}

#[derive(Clone, Debug)]
pub struct DeleteLineOp {
    pub y_pos: usize,
    pub content: String,
    pub cursor_to: Position,
    pub cursor_from: Position,
}
impl DeleteLineOp {
    pub fn invert(self) -> InsertLineOp {
        InsertLineOp {
            y_pos: self.y_pos,
            content: self.content,
            cursor_to: self.cursor_to,
            cursor_from: self.cursor_from,
        }
    }
}

#[derive(Clone, Debug)]
pub struct DeleteVisualOp {
    pub selection: VisualSelection,
    pub deleted: Option<Content>,
}
impl DeleteVisualOp {
    pub fn invert(self) -> InsertVisualOp {
        let (start, _) = super::selection_absolute_order(&self.selection);
        InsertVisualOp {
            selection: self.selection,
            content: self.deleted.unwrap(),
            cursor_to: start,
            cursor_from: start,
        }
    }
}

#[derive(Clone, Debug)]
pub enum EditOp {
    Insert(InsertOp),
    InsertSingle(InsertSingleOp),
    InsertLine(InsertLineOp),
    Delete(DeleteOp),
    DeleteLine(DeleteLineOp),
    DeleteBack(DeleteBackOp),
    DeleteForward(DeleteForwardOp),
    DeleteX(DeleteXOp),
    DeleteVisual(DeleteVisualOp),
    InsertVisual(InsertVisualOp),
    Split(SplitOp),
    Join(JoinOp),
}
impl EditOp {
    pub fn invert(self) -> EditOp {
        match self {
            EditOp::Insert(insert_op) => {
                insert_op.invert().into()
            }
            EditOp::InsertSingle(insert_single_op) => {
                insert_single_op.invert().into()
            }
            EditOp::InsertVisual(insert_visual_op) => {
                insert_visual_op.invert().into()
            }
            EditOp::DeleteBack(delete_op) => {
                delete_op.invert().into()
            }
            EditOp::Split(split_op) => {
                split_op.invert().into()
            }
            EditOp::Join(join_op) => {
                join_op.invert().into()
            }
            EditOp::Delete(delete_op) => {
                delete_op.invert().into()
            }
            EditOp::DeleteForward(delete_fw_op) => {
                delete_fw_op.invert().into()
            }
            EditOp::DeleteX(delete_op) => {
                delete_op.invert().into()
            }
            EditOp::DeleteVisual(delete_visual_op) => {
                delete_visual_op.invert().into()
            }
            EditOp::DeleteLine(delete_line_op) => {
                delete_line_op.invert().into()
            }
            EditOp::InsertLine(insert_line_op) => {
                insert_line_op.invert().into()
            }
        }
    }
}
impl From<InsertOp> for EditOp {
    fn from(insert_op: InsertOp) -> Self {
        EditOp::Insert(insert_op)
    }
}
impl From<InsertSingleOp> for EditOp {
    fn from(insert_single_op: InsertSingleOp) -> Self {
        EditOp::InsertSingle(insert_single_op)
    }
}
impl From<InsertVisualOp> for EditOp {
    fn from(insert_visual_op: InsertVisualOp) -> Self {
        EditOp::InsertVisual(insert_visual_op)
    }
}
impl From<InsertLineOp> for EditOp {
    fn from(insert_line_op: InsertLineOp) -> Self {
        EditOp::InsertLine(insert_line_op)
    }
}
impl From<DeleteOp> for EditOp {
    fn from(delete_op: DeleteOp) -> Self {
        EditOp::Delete(delete_op)
    }
}
impl From<DeleteLineOp> for EditOp {
    fn from(delete_line_op: DeleteLineOp) -> Self {
        EditOp::DeleteLine(delete_line_op)
    }
}
impl From<DeleteBackOp> for EditOp {
    fn from(delete_op: DeleteBackOp) -> Self {
        EditOp::DeleteBack(delete_op)
    }
}
impl From<DeleteVisualOp> for EditOp {
    fn from(delete_op: DeleteVisualOp) -> Self {
        EditOp::DeleteVisual(delete_op)
    }
}
impl From<SplitOp> for EditOp {
    fn from(split_op: SplitOp) -> Self {
        EditOp::Split(split_op)
    }
}
impl From<JoinOp> for EditOp {
    fn from(join_op: JoinOp) -> Self {
        EditOp::Join(join_op)
    }
}
impl From<DeleteForwardOp> for EditOp {
    fn from(delete_op: DeleteForwardOp) -> Self {
        EditOp::DeleteForward(delete_op)
    }
}
impl From<DeleteXOp> for EditOp {
    fn from(delete_op: DeleteXOp) -> Self {
        EditOp::DeleteX(delete_op)
    }
}