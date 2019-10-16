use crate::{Stream, Parser};

pub trait IntoParser<S: Stream> {
    type Target;
    type Parser: Parser<S, Target=Self::Target>;
    fn into_parser(self) -> Self::Parser;
}

impl<S: Stream, P: Parser<S>> IntoParser<S> for P {
    type Target = P::Target;
    type Parser = P;

    fn into_parser(self) -> Self::Parser {
        self
    }
}

