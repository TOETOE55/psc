use crate::core::err::ParseMsg;
use crate::core::state::ParseState;
use crate::core::traits::parser::Parser;
use crate::core::traits::stream::Stream;
use std::marker::PhantomData;
use std::str::Chars;
use crate::core::traits::covert::IntoParser;

/// Satisfy parser
#[derive(Clone)]
pub struct Satisfy<S, F> {
    satisfy: F,
    _s: PhantomData<S>,
}

impl<S, F> Satisfy<S, F> {
    pub fn new(satisfy: F) -> Self {
        Self {
            satisfy,
            _s: PhantomData,
        }
    }
}

impl<S: Stream, F> Parser<S> for Satisfy<S, F>
where
    F: Fn(&S::Item) -> bool,
{
    type Target = S::Item;
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        stream
            .next()
            .filter(|ch| (self.satisfy)(ch))
            .ok_or(ParseMsg::UnExcept(format!("unexpected token")))
    }
}

pub fn satisfy<S: Stream, F>(f: F) -> Satisfy<S, F>
where
    F: Fn(&S::Item) -> bool,
{
    Satisfy::new(f)
}

/// Char matching parser
#[derive(Clone)]
pub struct Char<S> {
    pub(crate) ch: char,
    _s: PhantomData<S>,
}

impl<S> Char<S> {
    pub fn new(ch: char) -> Self {
        Self {
            ch,
            _s: PhantomData,
        }
    }
}

impl<'a> Parser<Chars<'a>> for Char<Chars<'a>> {
    type Target = char;
    fn parse(&self, stream: &mut Chars<'a>) -> Result<Self::Target, ParseMsg> {
        let ch = stream.next().ok_or(ParseMsg::EOF)?;
        if self.ch == ch {
            Ok(ch)
        } else {
            Err(ParseMsg::Except(format!(
                "expected {}, found {}.",
                self.ch, ch
            )))
        }
    }
}

impl<'a> Parser<ParseState<'a>> for Char<ParseState<'a>> {
    type Target = char;
    fn parse(&self, stream: &mut ParseState<'a>) -> Result<Self::Target, ParseMsg> {
        let ch = stream.next().ok_or(ParseMsg::EOF)?;
        if self.ch == ch {
            Ok(ch)
        } else {
            Err(ParseMsg::Except(format!(
                "expected {}, found {} at {:?}.",
                self.ch, ch, stream.pos
            )))
        }
    }
}

impl<'a> IntoParser<Chars<'a>> for char {
    type Target = char;
    type Parser = Char<Chars<'a>>;

    fn into_parser(self) -> Self::Parser {
        Char::new(self)
    }
}

impl<'a> IntoParser<ParseState<'a>> for char {
    type Target = char;
    type Parser = Char<ParseState<'a>>;

    fn into_parser(self) -> Self::Parser {
        Char::new(self)
    }
}

pub fn char<S>(ch: char) -> Char<S> {
    Char::new(ch)
}

/// String matching parser
#[derive(Clone)]
pub struct Strg<'a, S> {
    pub(crate) s: &'a str,
    _s: PhantomData<S>,
}

impl<'a, S> Strg<'a, S> {
    pub fn new(s: &'a str) -> Self {
        Strg { s, _s: PhantomData }
    }
}

impl<'a, 's> Parser<Chars<'s>> for Strg<'a, Chars<'s>> {
    type Target = &'s str;
    fn parse(&self, stream: &mut Chars<'s>) -> Result<Self::Target, ParseMsg> {
        let re = regex::Regex::new(self.s).unwrap();
        let src = stream.as_str();
        match re.find(src) {
            Some(range) if range.start() == 0 => {
                let (matched, rest) = src.split_at(range.end());
                *stream = rest.chars();
                Ok(matched)
            },
            _ => Err(ParseMsg::Except(format!("expected {}", self.s))),
        }
    }
}

impl<'a, 's> Parser<ParseState<'s>> for Strg<'a, ParseState<'s>> {
    type Target = &'s str;
    fn parse(&self, stream: &mut ParseState<'s>) -> Result<Self::Target, ParseMsg> {
        let re = regex::Regex::new(self.s).unwrap();
        let src = stream.as_str();
        match re.find(src) {
            Some(range) if range.start() == 0 => {
                let (matched, rest) = src.split_at(range.end());
                stream.src = rest.chars();
                Ok(matched)
            },
            _ => Err(ParseMsg::Except(format!(
                "expected {} at {:?}",
                self.s, stream.pos
            ))),
        }
    }
}

impl<'a, 's> IntoParser<Chars<'s>> for &'a str {
    type Target = &'s str;
    type Parser = Strg<'a, Chars<'s>>;

    fn into_parser(self) -> Self::Parser {
        Strg::new(self)
    }
}

impl<'a, 's> IntoParser<ParseState<'s>> for &'a str {
    type Target = &'s str;
    type Parser = Strg<'a, ParseState<'s>>;

    fn into_parser(self) -> Self::Parser {
        Strg::new(self)
    }
}

pub fn strg<S>(s: &str) -> Strg<S> {
    Strg::new(s)
}
