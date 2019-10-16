use crate::core::err::ParseMsg;
use crate::core::state::ParseState;
use crate::core::traits::covert::IntoParser;
use crate::core::traits::parser::Parser;
use crate::core::traits::stream::Stream;
use std::marker::PhantomData;
use std::str::Chars;

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
            }
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
                for _ in 0..range.end() {
                    stream.next();
                }
                Ok(range.as_str())
            }
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

/// regex
/// ```
/// use psc::{Parser, reg};
/// let re = reg("(0+)(1*)");
/// let mut src = "0000111112".chars();
/// let res = re.parse(&mut src).unwrap();
/// assert_eq!(res, "000011111");
/// ```

pub struct Regex<S> {
    delegate: regex::Regex,
    _s: PhantomData<S>,
}

impl<S> Regex<S> {
    pub fn new(re: &str) -> Result<Self, regex::Error> {
        regex::Regex::new(re).map(|delegate| Self {
            delegate,
            _s: PhantomData,
        })
    }

    pub fn delegate(&self) -> &regex::Regex {
        &self.delegate
    }

    pub fn unwrap(self) -> regex::Regex {
        self.delegate
    }
}

impl<S: Stream> From<regex::Regex> for Regex<S> {
    fn from(re: regex::Regex) -> Self {
        Self {
            delegate: re,
            _s: PhantomData,
        }
    }
}

impl<'s> Parser<Chars<'s>> for Regex<Chars<'s>> {
    type Target = &'s str;
    fn parse(&self, stream: &mut Chars<'s>) -> Result<Self::Target, ParseMsg> {
        let src = stream.as_str();
        match self.delegate.find(src) {
            Some(range) if range.start() == 0 => {
                let (matched, rest) = src.split_at(range.end());
                *stream = rest.chars();
                Ok(matched)
            }
            _ => Err(ParseMsg::Except(format!(
                "expected {}",
                self.delegate.as_str()
            ))),
        }
    }
}

impl<'s> Parser<ParseState<'s>> for Regex<ParseState<'s>> {
    type Target = &'s str;

    fn parse(&self, stream: &mut ParseState<'s>) -> Result<Self::Target, ParseMsg> {
        let src = stream.as_str();
        match self.delegate.find(src) {
            Some(range) if range.start() == 0 => {
                for _ in 0..range.end() {
                    stream.next();
                }
                Ok(range.as_str())
            }
            _ => Err(ParseMsg::Except(format!(
                "expected {} at {:?}",
                self.delegate.as_str(),
                stream.pos
            ))),
        }
    }
}

pub fn reg<S: Stream>(re: &str) -> Regex<S> {
    Regex::new(re).unwrap()
}
