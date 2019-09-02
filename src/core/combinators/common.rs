use crate::core::traits::parser::Parser;
use crate::core::traits::stream::Stream;

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

impl<S: Stream<Item=char>, F> Parser<S> for Satisfy<F>
    where F: Fn(&char) -> bool,
{
    type Target = char;
    fn parse(&self, stream : S) -> Option<(Self::Target, S)> {
        stream.next().filter(|(ch, _)| (self.satisfy)(ch))
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

impl<S: Stream<Item=char>> Parser<S> for Char {
    type Target = char;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)> {
        stream.next().filter(|&(ch, _)| self.ch == ch)
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

impl<'a, S: Stream<Item=char> + Clone> Parser<S> for Strg<'a> {
    type Target = &'a str;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)> {
        let mut chars = self.s.chars();
        let mut stream = stream;
        while let Some(ch) = chars.next() {
            stream = char(ch).parse(stream.clone())?.1;
        }
        Some((self.s, stream))
    }
}

pub fn strg(s: &str) -> Strg {
    Strg::new(s)
}