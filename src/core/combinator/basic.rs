use crate::core::traits::stream::{ParseState, Pos};
use crate::{Msg, ParseLogger, Parser};

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
#[derive(Clone, Copy, Debug)]
pub struct Char {
    pub(crate) ch: char,
}

impl Char {
    pub fn new(ch: char) -> Self {
        Self { ch }
    }
}

impl<'a> Parser<ParseState<'a>> for Char {
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

pub fn char(ch: char) -> Char {
    Char::new(ch)
}

/// String matching parser
#[derive(Clone, Debug)]
pub struct Strg {
    pub(crate) temp: String,
}

impl Strg {
    pub fn new(temp: &str) -> Self {
        Strg {
            temp: temp.to_owned(),
        }
    }
}

impl<'s> Parser<ParseState<'s>> for Strg {
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

pub fn strg(s: &str) -> Strg {
    Strg::new(s)
}

/// regex
#[derive(Clone, Debug)]
pub struct Regex {
    delegate: regex::Regex,
}

impl Regex {
    pub fn new(re: &str) -> Result<Self, regex::Error> {
        regex::Regex::new(re).map(|delegate| Self { delegate })
    }

    pub fn delegate(&self) -> &regex::Regex {
        &self.delegate
    }

    pub fn unwrap(self) -> regex::Regex {
        self.delegate
    }
}

impl From<regex::Regex> for Regex {
    fn from(re: regex::Regex) -> Self {
        Self { delegate: re }
    }
}

impl<'s> Parser<ParseState<'s>> for Regex {
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

pub fn reg(re: &str) -> Regex {
    Regex::new(re).unwrap()
}

#[derive(Copy, Clone, Debug)]
pub struct EOF;

impl<'a> Parser<ParseState<'a>> for EOF {
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

#[derive(Copy, Clone, Debug)]
pub struct GetPos;
impl<'a> Parser<ParseState<'a>> for GetPos {
    type Target = Pos;

    fn parse(&self, stream: &mut ParseState<'a>, _: &mut ParseLogger) -> Option<Self::Target> {
        Some(stream.pos)
    }
}

pub fn pos() -> GetPos {
    GetPos
}
