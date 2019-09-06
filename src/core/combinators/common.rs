use crate::core::err::ParseMsg;
use crate::core::state::ParseState;
use crate::core::traits::parser::Parser;
use crate::core::traits::stream::Stream;
use std::marker::PhantomData;

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
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        stream
            .next()
            .filter(|(ch, _)| (self.satisfy)(ch))
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
    ch: char,
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

impl<'a> Parser<&'a str> for Char<&'a str> {
    type Target = char;
    fn parse(&self, stream: &'a str) -> Result<(Self::Target, &'a str), ParseMsg> {
        let (ch, s) = stream.next().ok_or(ParseMsg::EOF)?;
        if self.ch == ch {
            Ok((ch, s))
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
    fn parse(&self, stream: ParseState<'a>) -> Result<(Self::Target, ParseState<'a>), ParseMsg> {
        let (ch, ps) = stream.next().ok_or(ParseMsg::EOF)?;
        if self.ch == ch {
            Ok((ch, ps))
        } else {
            Err(ParseMsg::Except(format!(
                "expected {}, found {} at {:?}.",
                self.ch, ch, ps.pos
            )))
        }
    }
}

pub fn char<S>(ch: char) -> Char<S> {
    Char::new(ch)
}

/// String matching parser
#[derive(Clone)]
pub struct Strg<'a, S> {
    s: &'a str,
    _s: PhantomData<S>,
}

impl<'a, S> Strg<'a, S> {
    pub fn new(s: &'a str) -> Self {
        Strg { s, _s: PhantomData }
    }
}

impl<'a, 's> Parser<&'s str> for Strg<'a, &'s str> {
    type Target = &'s str;
    fn parse(&self, stream: &'s str) -> Result<(Self::Target, &'s str), ParseMsg> {
        match stream.match_indices(self.s).next() {
            Some((0, matched)) => {
                let mut chars = self.s.chars();
                let mut stream = stream;
                while let Some(_) = chars.next() {
                    stream = stream.next().unwrap().1;
                }
                Ok((matched, stream))
            }
            _ => Err(ParseMsg::Except(format!("expected {}", self.s))),
        }
    }
}

impl<'a, 's> Parser<ParseState<'s>> for Strg<'a, ParseState<'s>> {
    type Target = &'s str;
    fn parse(&self, stream: ParseState<'s>) -> Result<(Self::Target, ParseState<'s>), ParseMsg> {
        match stream.src.match_indices(self.s).next() {
            Some((0, matched)) => {
                let mut chars = self.s.chars();
                let mut stream = stream;
                while let Some(_) = chars.next() {
                    stream = stream.next().unwrap().1;
                }
                Ok((matched, stream))
            }
            _ => Err(ParseMsg::Except(format!(
                "expected {} at {:?}",
                self.s, stream.pos
            ))),
        }
    }
}

pub fn strg<S>(s: &str) -> Strg<S> {
    Strg::new(s)
}
