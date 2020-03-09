use crate::{ParseErr, Parser};

pub trait IntoParser<S, E = ParseErr> {
    type Target;
    type Parser: Parser<S, E, Target = Self::Target>;
    fn into_parser(self) -> Self::Parser;
}

impl<S, E, P: Parser<S, E>> IntoParser<S, E> for P {
    type Target = P::Target;
    type Parser = P;

    fn into_parser(self) -> Self::Parser {
        self
    }
}
