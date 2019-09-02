use crate::core::state::Pos;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ParseError {
    msg: String,
    pos: Pos,
}

impl ParseError {
    pub fn new(msg: String, pos: Pos) -> Self {
        ParseError { msg, pos }
    }
}