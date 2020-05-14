use crate::core::traits::stream::{ParseState, Pos};
use crate::{Msg, ParseLogger, Parser};
use std::marker::PhantomData;

/// Satisfy parser
#[derive(Clone, Copy, Debug)]
pub struct Satisfy<F> {
    satisfy: F,
}

impl<F> Satisfy<F> {
    pub fn new(satisfy: F) -> Self {
        Self { satisfy }
    }
}

impl<'a, F> Parser<ParseState<'a>> for Satisfy<F>
where
    F: Fn(&char) -> bool,
{
    type Target = char;

    fn parse(&self, stream: &mut ParseState<'a>, logger: &mut ParseLogger) -> Option<Self::Target> {
        match stream.next() {
            None => {
                logger.with(Msg::Err(format!(
                    "error at {:?}, input exhaustive.",
                    stream.pos
                )));
                None
            }
            Some(ch) => {
                if (self.satisfy)(&ch) {
                    Some(ch)
                } else {
                    logger.with(Msg::Err(format!("error at {:?}.", stream.pos)));
                    None
                }
            }
        }
    }
}

pub fn satisfy<F>(f: F) -> Satisfy<F>
where
    F: Fn(&char) -> bool,
{
    Satisfy::new(f)
}

/// Char matching parser
#[derive(Debug)]
pub struct Char<S> {
    pub(crate) ch: char,
    _marker: PhantomData<fn(&mut S)>
}

impl<S> Char<S> {
    pub fn new(ch: char) -> Self {
        Self { ch, _marker: PhantomData }
    }
}

impl<S> Clone for Char<S> {
    fn clone(&self) -> Self {
        Self { ch: self.ch, _marker: PhantomData }
    }
}

impl<S> Copy for Char<S> { }

impl<'a> Parser<ParseState<'a>> for Char<ParseState<'a>> {
    type Target = char;

    fn parse(&self, stream: &mut ParseState<'a>, logger: &mut ParseLogger) -> Option<Self::Target> {
        match stream.next() {
            None => {
                logger.with(Msg::Err(format!(
                    "error at {:?}, input exhaustive.",
                    stream.pos
                )));
                None
            }
            Some(ch) => {
                if ch == self.ch {
                    Some(ch)
                } else {
                    logger.with(Msg::Err(format!(
                        "error at {:?}, expecting '{}', unexpected '{}'.",
                        stream.pos, self.ch, ch
                    )));
                    None
                }
            }
        }
    }
}

pub fn char<S>(ch: char) -> Char<S> {
    Char::new(ch)
}

/// String matching parser
#[derive(Debug)]
pub struct Strg<S> {
    pub(crate) temp: String,
    _marker: PhantomData<fn(&mut S)>
}

impl<S> Strg<S> {
    pub fn new(temp: &str) -> Self {
        Strg {
            temp: temp.to_owned(),
            _marker: PhantomData
        }
    }
}

impl<S> Clone for Strg<S> {
    fn clone(&self) -> Self {
        Self { temp: self.temp.clone(), _marker: PhantomData }
    }
}


impl<'s> Parser<ParseState<'s>> for Strg<ParseState<'s>> {
    type Target = &'s str;

    fn parse(&self, stream: &mut ParseState<'s>, logger: &mut ParseLogger) -> Option<Self::Target> {
        let src = stream.as_str();
        if let Some(0) = src.find(&self.temp) {
            stream.take(self.temp.len()).for_each(|_| {});
            Some(&src[0..self.temp.len()])
        } else {
            logger.with(Msg::Err(format!(
                "error at {:?}, expecting \"{}\".",
                stream.pos, self.temp
            )));
            None
        }
    }
}

pub fn strg<S>(s: &str) -> Strg<S> {
    Strg::new(s)
}

/// regex
#[derive(Debug)]
pub struct Regex<S> {
    delegate: regex::Regex,
    _marker: PhantomData<fn(&mut S)>
}

impl<S> Clone for Regex<S> {
    fn clone(&self) -> Self {
        Self { delegate: self.delegate.clone(), _marker: PhantomData }
    }
}


impl<S> Regex<S> {
    pub fn new(re: &str) -> Result<Self, regex::Error> {
        regex::Regex::new(re).map(|delegate| Self { delegate, _marker: PhantomData })
    }

    pub fn delegate(&self) -> &regex::Regex {
        &self.delegate
    }

    pub fn unwrap(self) -> regex::Regex {
        self.delegate
    }
}

impl<S> From<regex::Regex> for Regex<S> {
    fn from(re: regex::Regex) -> Self {
        Self { delegate: re, _marker: PhantomData }
    }
}

impl<'s> Parser<ParseState<'s>> for Regex<ParseState<'s>> {
    type Target = &'s str;

    fn parse(&self, stream: &mut ParseState<'s>, logger: &mut ParseLogger) -> Option<Self::Target> {
        let src = stream.as_str();
        match self.delegate.find(src) {
            Some(m) if m.start() == 0 => {
                stream.take(m.end()).for_each(|_| {});
                Some(&src[0..m.end()])
            }
            _ => {
                logger.with(Msg::Err(format!(
                    "error at {:?}, expecting \"{}\".",
                    stream.pos,
                    self.delegate.as_str()
                )));
                None
            }
        }
    }
}

pub fn reg<S>(re: &str) -> Regex<S> {
    Regex::new(re).unwrap()
}

#[derive(Debug)]
pub struct EOF<S> {
    _marker: PhantomData<fn(&mut S)>
}

impl<'a> Parser<ParseState<'a>> for EOF<ParseState<'a>> {
    type Target = ();

    fn parse(&self, stream: &mut ParseState<'a>, logger: &mut ParseLogger) -> Option<Self::Target> {
        match stream.next() {
            None => Some(()),
            Some(ch) => {
                logger.with(Msg::Err(format!(
                    "error at {:?}, expecting eof, unexpected '{}'.",
                    stream.pos, ch
                )));
                None
            }
        }
    }
}

pub fn eof<S>() -> EOF<S> {
    EOF { _marker: PhantomData }
}

impl<S> Clone for EOF<S> {
    fn clone(&self) -> Self {
        Self { _marker: PhantomData }
    }
}

impl<S> Copy for EOF<S> { }

#[derive(Debug)]
pub struct GetPos<S> {
    _marker: PhantomData<fn(&mut S)>
}

impl<S> Clone for GetPos<S> {
    fn clone(&self) -> Self {
        Self { _marker: PhantomData }
    }
}

impl<S> Copy for GetPos<S> { }

impl<'a> Parser<ParseState<'a>> for GetPos<ParseState<'a>> {
    type Target = Pos;

    fn parse(&self, stream: &mut ParseState<'a>, _: &mut ParseLogger) -> Option<Self::Target> {
        Some(stream.pos)
    }
}

pub fn pos<S>() -> GetPos<S> {
    GetPos {
        _marker: PhantomData
    }
}