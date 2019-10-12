use crate::core::traits::stream::Stream;
use std::str::Chars;

#[derive(Clone, Debug)]
pub struct ParseState<'a> {
    pub(crate) src: Chars<'a>,
    pub(crate) pos: Pos,
    pub(crate) len: usize,
    pub(crate) idx: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Pos {
    pub col: usize,
    pub row: usize,
}

impl<'a> ParseState<'a> {
    pub fn new(src: &'a str) -> Self {
        ParseState {
            src: src.chars(),
            pos: Pos { col: 0, row: 0 },
            len: src.len(),
            idx: 0,
        }
    }

    pub fn as_str(&self) -> &'a str {
        self.src.as_str()
    }

    pub fn pos(&self) -> Pos {
        self.pos
    }

    pub fn index(&self) -> usize {
        self.idx
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

impl<'a> Iterator for ParseState<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.src.next()?;
        let pos = match ch {
            '\n' => Pos {
                row: self.pos.row + 1,
                ..self.pos
            },
            '\t' => Pos {
                col: self.pos.col + 8 - (self.pos.col - 1) % 8,
                ..self.pos
            },
            _ => Pos {
                col: self.pos.col + 1,
                ..self.pos
            },
        };
        self.pos = pos;
        self.idx += 1;
        self.len -= 1;
        Some(ch)
    }
}

impl<'a> Stream for ParseState<'a> {}
