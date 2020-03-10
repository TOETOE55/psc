use crate::core::traits::covert::IntoParser;
use crate::{Parser, satisfy, ParserExt, wrap};
use crate::core::traits::stream::ParseState;

pub fn alpha<'a>() -> impl Parser<ParseState<'a>, Target=char> + Clone {
    satisfy(|ch| ch.is_alphabetic()).info("expecting alpha")
}

pub fn digit<'a>() -> impl Parser<ParseState<'a>, Target=char> + Clone {
    satisfy(|ch| ch.is_digit(10)).info("expecting digit")
}

pub fn blank<'a>() -> impl Parser<ParseState<'a>, Target=char> + Copy {
    wrap(' ') | '\n' | '\t' | '\r'
}

pub fn lexeme<'a, A>(p: impl IntoParser<ParseState<'a>, Target=A>) -> impl Parser<ParseState<'a>, Target=A> {
    wrap(blank().many_()) >> p << blank().many_()
}