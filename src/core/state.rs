use crate::core::traits::stream::Stream;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseState<'a> {
    pub src: &'a str,
    pub col: usize,
    pub row: usize,
}

impl<'a> ParseState<'a> {
    pub fn new(src: &'a str) -> Self {
        ParseState {
            src,
            col: 0,
            row: 0,
        }
    }
}


impl<'a> Stream for ParseState<'a> {
    type Item = char;
    fn next(&self) -> Option<(Self::Item, Self)> {
        self.src.next().map(|(ch, src)| (ch, match ch {
            '\n' => Self {
                src,
                row: self.row + 1,
                ..self.clone()
            },
            '\t' => Self {
                src,
                col: self.col+8 - (self.col-1)%8,
                ..self.clone()
            },
            _ => Self {
                src,
                col: self.col + 1,
                ..self.clone()
            }
        }))
    }
}