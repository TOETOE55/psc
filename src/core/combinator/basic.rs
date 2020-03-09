use crate::adaptor::Info;
use crate::covert::IntoParser;
use crate::err::ParserLogger;
use crate::{Consumed, ParseErr, ParseState, Parser};
use std::marker::PhantomData;

#[derive(Debug)]
pub struct Satisfy<E, F> {
    satisfy: F,
    _marker: PhantomData<fn(&mut E)>,
}

impl<E, F> Satisfy<E, F> {
    pub fn new(satisfy: F) -> Self {
        Self {
            satisfy,
            _marker: PhantomData,
        }
    }
}

impl<E, F: Clone> Clone for Satisfy<E, F> {
    fn clone(&self) -> Self {
        Self {
            satisfy: self.satisfy.clone(),
            _marker: PhantomData,
        }
    }
}

impl<E, F: Copy> Copy for Satisfy<E, F> {}

impl<'a, E, F> Parser<ParseState<'a>, E> for Satisfy<E, F>
where
    E: ParserLogger,
    F: Fn(&char) -> bool,
{
    type Target = <ParseState<'a> as Iterator>::Item;

    fn parse(&self, s: &mut ParseState<'a>, err: &mut E) -> Consumed<Option<Self::Target>> {
        match s.next() {
            Some(ch) => {
                if (self.satisfy)(&ch) {
                    Consumed::Some(Some(ch))
                } else {
                    err.clear();
                    err.err(&format!("err at {:?}", s.pos));
                    Consumed::Empty(None)
                }
            }
            None => {
                err.clear();
                err.err(&format!("err at {:?} input exhausted", s.pos));
                Consumed::Empty(None)
            }
        }
    }
}

pub fn satisfy<E, F>(satisfy: F) -> Satisfy<E, F>
where
    E: ParserLogger,
    F: Fn(&char) -> bool,
{
    Satisfy::new(satisfy)
}

#[derive(Debug)]
pub struct Char<E> {
    ch: char,
    _marker: PhantomData<fn(&mut E)>,
}

impl<E> Char<E> {
    pub fn new(ch: char) -> Self {
        Self {
            ch,
            _marker: PhantomData,
        }
    }
}

impl<E> Clone for Char<E> {
    fn clone(&self) -> Self {
        Self {
            ch: self.ch,
            _marker: PhantomData,
        }
    }
}

impl<E> Copy for Char<E> {}

impl<'a, E: ParserLogger> Parser<ParseState<'a>, E> for Char<E> {
    type Target = char;

    fn parse(&self, s: &mut ParseState<'a>, err: &mut E) -> Consumed<Option<Self::Target>> {
        match s.next() {
            Some(ch) => {
                if self.ch == ch {
                    Consumed::Some(Some(ch))
                } else {
                    err.clear();
                    err.err(&format!(
                        "err at {:?}, unexpect '{}' expecting '{}'",
                        s.pos, ch, self.ch
                    ));
                    Consumed::Empty(None)
                }
            }
            None => {
                err.clear();
                err.err(&format!("err at {:?} input exhausted", s.pos));
                Consumed::Empty(None)
            }
        }
    }
}

pub fn char<E: ParserLogger>(ch: char) -> Char<E> {
    Char::new(ch)
}

#[derive(Debug)]
pub struct Strg<E> {
    temp: String,
    _marker: PhantomData<fn(&mut E) -> &str>,
}

impl<E> Strg<E> {
    pub fn new(temp: &str) -> Self {
        Self {
            temp: temp.to_string(),
            _marker: PhantomData,
        }
    }
}

impl<E> Clone for Strg<E> {
    fn clone(&self) -> Self {
        Self {
            temp: self.temp.to_owned(),
            _marker: PhantomData,
        }
    }
}

impl<'a, E: ParserLogger> Parser<ParseState<'a>, E> for Strg<E> {
    type Target = &'a str;

    fn parse(&self, s: &mut ParseState<'a>, err: &mut E) -> Consumed<Option<Self::Target>> {
        let src = s.as_str();
        if let Some(0) = src.find(&self.temp) {
            s.take(self.temp.len()).for_each(|_| {});
            Consumed::Some(Some(src.split_at(self.temp.len()).0))
        } else {
            err.clear();
            err.err(&format!(
                "error at {:?}, expecting \"{}\"",
                s.pos, self.temp
            ));
            Consumed::Empty(None)
        }
    }
}

pub fn strg<E: ParserLogger>(temp: &str) -> Strg<E> {
    Strg::new(temp)
}

pub struct EOF<E> {
    _marker: PhantomData<fn(&mut E)>,
}

impl<E> EOF<E> {
    pub fn new() -> Self {
        Self {
            _marker: PhantomData,
        }
    }
}

impl<E> Clone for EOF<E> {
    fn clone(&self) -> Self {
        Self::new()
    }
}

impl<E> Copy for EOF<E> {}

impl<E> Default for EOF<E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, E: ParserLogger> Parser<ParseState<'a>, E> for EOF<E> {
    type Target = ();

    fn parse(&self, s: &mut ParseState<'a>, err: &mut E) -> Consumed<Option<Self::Target>> {
        match s.next() {
            None => Consumed::Empty(Some(())),
            Some(x) => {
                err.clear();
                err.err(&format!(
                    "error at {:?}, expecting eof, unexpected '{}'",
                    s.pos, x
                ));
                Consumed::Empty(None)
            }
        }
    }
}

pub fn eof<E: ParserLogger>() -> EOF<E> {
    EOF::new()
}

impl<'a> IntoParser<ParseState<'a>> for char {
    type Target = char;
    type Parser = Char<ParseErr>;

    fn into_parser(self) -> Self::Parser {
        Char::new(self)
    }
}

impl<'a> IntoParser<ParseState<'a>> for &str {
    type Target = &'a str;
    type Parser = Strg<ParseErr>;

    fn into_parser(self) -> Self::Parser {
        Strg::new(self)
    }
}

pub struct Regex<E> {
    delegate: regex::Regex,
    _marker: PhantomData<fn(&mut E)>,
}

impl<S> Regex<S> {
    pub fn new(re: &str) -> Result<Self, regex::Error> {
        regex::Regex::new(re).map(|delegate| Self {
            delegate,
            _marker: PhantomData,
        })
    }

    pub fn delegate(&self) -> &regex::Regex {
        &self.delegate
    }

    pub fn unwrap(self) -> regex::Regex {
        self.delegate
    }
}

impl<E> From<regex::Regex> for Regex<E> {
    fn from(re: regex::Regex) -> Self {
        Self {
            delegate: re,
            _marker: PhantomData,
        }
    }
}

impl<'a, E: ParserLogger> Parser<ParseState<'a>, E> for Regex<E> {
    type Target = &'a str;

    fn parse(&self, s: &mut ParseState<'a>, err: &mut E) -> Consumed<Option<Self::Target>> {
        let re = &self.delegate;
        let src = s.as_str();
        match re.find(src) {
            Some(m) if m.start() == 0 => {
                s.take(m.end()).for_each(|_| {});
                Consumed::Some(Some(m.as_str()))
            }
            _ => {
                err.clear();
                err.err(&format!(
                    "error at {:?}, expecting \"{}\"",
                    s.pos,
                    self.delegate.as_str()
                ));
                Consumed::Empty(None)
            }
        }
    }
}

pub fn reg<E: ParserLogger>(reg: &str) -> Regex<E> {
    Regex::new(reg).unwrap()
}
