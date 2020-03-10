use crate::combinator::basic::{Char, Strg};
use crate::traits::stream::ParseState;
use crate::Parser;

pub trait IntoParser<S> {
    type Target;
    type Parser: Parser<S, Target = Self::Target>;
    fn into_parser(self) -> Self::Parser;
}

impl<S, P: Parser<S>> IntoParser<S> for P {
    type Target = P::Target;
    type Parser = P;

    fn into_parser(self) -> Self::Parser {
        self
    }
}

impl<'a> IntoParser<ParseState<'a>> for char {
    type Target = char;
    type Parser = Char;

    fn into_parser(self) -> Self::Parser {
        Char::new(self)
    }
}

impl<'s> IntoParser<ParseState<'s>> for &str {
    type Target = &'s str;
    type Parser = Strg;

    fn into_parser(self) -> Self::Parser {
        Strg::new(self)
    }
}
