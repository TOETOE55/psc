use crate::core::traits::stream::ParseState;
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
            Some(src.split_at(self.temp.len()).0)
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
/// ```
/// use psc::{Parser, reg};
/// let re = reg("(0+)(1*)");
/// let mut src = "0000111112".chars();
/// let res = re.parse(&mut src).unwrap();
/// assert_eq!(res, "000011111");
/// ```

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
                Some(src.split_at(m.end()).0)
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
                    stream.pos,
                    ch
                )));
                None
            },
        }
    }
}