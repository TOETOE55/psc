use crate::core::traits::stream::Stream;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseState<'a> {
    pub src: &'a str,
    pub pos: Pos,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Pos {
    pub col: usize,
    pub row: usize,
}

impl<'a> ParseState<'a> {
    pub fn new(src: &'a str) -> Self {
        ParseState {
            src,
            pos: Pos {
                col: 0,
                row: 0,
            }
        }
    }
}


impl<'a> Stream for ParseState<'a> {
    type Item = char;
    fn next(&self) -> Option<(Self::Item, Self)> {
        self.src.next().map(|(ch, src)| (ch, Self {
            src,
            pos: match ch {
                '\n' => Pos {
                    row: self.pos.row + 1,
                    ..self.pos
                },
                '\t' => Pos {
                    col: self.pos.col+8 - (self.pos.col-1)%8,
                    ..self.pos
                },
                _ => Pos {
                    col: self.pos.col + 1,
                    ..self.pos
                },
            },
        }))
    }
}