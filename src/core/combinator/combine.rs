use crate::core::traits::covert::IntoParser;
use crate::core::traits::stream::ParseState;
use crate::{satisfy, wrap, Parser, ParserExt};

pub fn alpha() -> impl for<'a> Parser<ParseState<'a>, Target = char> + Clone {
    satisfy(|ch| ch.is_alphabetic()).info("expecting alpha")
}

pub fn digit() -> impl for<'a> Parser<ParseState<'a>, Target = char> + Clone {
    satisfy(|ch| ch.is_digit(10)).info("expecting digit")
}

pub fn blank() -> impl for<'a> Parser<ParseState<'a>, Target = char> + Copy {
    crate::char(' ').or('\n').or('\t').or('\r')
}

pub fn lexeme<'a, A>(
    p: impl IntoParser<ParseState<'a>, Target = A>,
) -> impl Parser<ParseState<'a>, Target = A> {
    wrap(blank().many_()) >> p << blank().many_()
}
