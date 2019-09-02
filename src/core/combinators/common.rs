use crate::core::traits::parser::Parser;
use crate::core::traits::stream::Stream;
use crate::core::state::ParseState;
use crate::core::err::ParseError;

/// satisfy
#[derive(Clone)]
pub struct Satisfy<F> {
    satisfy: F,
}

impl<F> Satisfy<F> {
    pub fn new(satisfy: F) -> Self {
        Self { satisfy }
    }
}

impl<'a, F> Parser<ParseState<'a>, ParseError> for Satisfy<F>
    where F: Fn(&char) -> bool,
{
    type Target = char;
    fn parse(&self, stream : ParseState<'a>) -> Result<(Self::Target, ParseState<'a>), ParseError> {
        stream.next().filter(|(ch, _)| (self.satisfy)(ch))
            .ok_or(ParseError::new(format!("dissatisfy at {:?}", stream.pos), stream.pos))
    }
}

impl<'a, F> Parser<&'a str, String> for Satisfy<F>
    where F: Fn(&char) -> bool,
{
    type Target = char;
    fn parse(&self, stream : &'a str) -> Result<(Self::Target, &'a str), String> {
        stream.next().filter(|(ch, _)| (self.satisfy)(ch))
            .ok_or("expected dissatisfy".to_string())
    }
}

pub fn satisfy<F>(f: F) -> Satisfy<F>
    where F: Fn(&char) -> bool,
{
    Satisfy::new(f)
}


/// char
#[derive(Clone)]
pub struct Char {
    ch: char,
}

impl Char {
    pub fn new(ch: char) -> Self {
        Self { ch }
    }
}

impl<'a> Parser<ParseState<'a>, ParseError> for Char {
    type Target = char;
    fn parse(&self, stream: ParseState<'a>) -> Result<(Self::Target, ParseState<'a>), ParseError> {
        stream.next().filter(|&(ch, _)| self.ch == ch)
            .ok_or(ParseError::new(format!("expected at {:?}", stream.pos), stream.pos))
    }
}

impl<'a> Parser<&'a str, String> for Char {
    type Target = char;
    fn parse(&self, stream: &'a str) -> Result<(Self::Target, &'a str), String> {
        stream.next().filter(|&(ch, _)| self.ch == ch)
            .ok_or(format!("expected isn't {}", self.ch))
    }
}

pub fn char(ch: char) -> Char {
    Char::new(ch)
}

/// strg
#[derive(Clone)]
pub struct Strg<'a> {
    s: &'a str,
}

impl<'a> Strg<'a> {
    pub fn new(s: &'a str) -> Self {
        Strg { s }
    }
}

impl<'a, 's> Parser<ParseState<'s>, ParseError> for Strg<'a> {
    type Target = &'a str;
    fn parse(&self, stream: ParseState<'s>) -> Result<(Self::Target, ParseState<'s>), ParseError> {
        let mut chars = self.s.chars();
        let mut stream = stream;
        while let Some(ch) = chars.next() {
            stream = char(ch).parse(stream.clone())?.1;
        }
        Ok((self.s, stream))
    }
}

impl<'a, 's> Parser<&'s str, String> for Strg<'a> {
    type Target = &'a str;
    fn parse(&self, stream: &'s str) -> Result<(Self::Target, &'s str), String> {
        let mut chars = self.s.chars();
        let mut stream = stream;
        while let Some(ch) = chars.next() {
            stream = char(ch).parse(stream.clone())?.1;
        }
        Ok((self.s, stream))
    }
}

pub fn strg(s: &str) -> Strg {
    Strg::new(s)
}