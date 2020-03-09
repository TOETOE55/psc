use crate::adaptor::Info;
use crate::basic::Satisfy;
use crate::covert::IntoParser;
use crate::err::ParserLogger;
use crate::{satisfy, wrap, ParseState, Parser, ParserExt};

pub fn letter<'a, E: ParserLogger>() -> impl Parser<ParseState<'a>, E, Target = char> {
    satisfy(|c| c.is_alphabetic()).info("expecting alphabetic")
}

pub fn digit<'a, E: ParserLogger>() -> impl Parser<ParseState<'a>, E, Target = char> {
    satisfy(|c| c.is_digit(10)).info("expecting digit")
}

pub fn blank<'a, E: ParserLogger>() -> impl Parser<ParseState<'a>, E, Target = char> {
    satisfy(|c| *c == ' ' || *c == '\n' || *c == '\t' || *c == '\r').info("expecting blank")
}

pub fn lexeme<'a, E, A>(
    p: impl IntoParser<ParseState<'a>, E, Target = A>,
) -> impl Parser<ParseState<'a>, E, Target = A>
where
    E: ParserLogger + Clone,
{
    wrap(blank().many_()) >> p.into_parser() << blank()
}
