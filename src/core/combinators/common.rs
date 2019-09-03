use crate::core::traits::parser::Parser;
use crate::core::traits::stream::Stream;
use crate::core::state::ParseState;
use crate::core::err::ParseMsg;
use std::marker::PhantomData;

/// satisfy
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
    where F: Fn(&S::Item) -> bool,
{
    type Target = S::Item;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg>  {
        stream.next().filter(|(ch, _)| (self.satisfy)(ch))
            .ok_or(ParseMsg::UnExcept(format!("unexpected char")))
    }
}


pub fn satisfy<S: Stream, F>(f: F) -> Satisfy<S, F>
    where F: Fn(&S::Item) -> bool,
{
    Satisfy::new(f)
}


/// char
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

impl<S: Stream<Item=char>> Parser<S> for Char<S> {
    type Target = char;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg>  {
        stream.next().filter(|&(ch, _)| self.ch == ch)
            .ok_or(ParseMsg::Except(format!("expected isn't {}", self.ch)))
    }
}

pub fn char<S>(ch: char) -> Char<S> {
    Char::new(ch)
}

/// strg
#[derive(Clone)]
pub struct Strg<'a, S> {
    s: &'a str,
    _s: PhantomData<S>,
}

impl<'a, S> Strg<'a, S> {
    pub fn new(s: &'a str) -> Self {
        Strg {
            s,
            _s: PhantomData,
        }
    }
}

impl<'a, S: Stream<Item=char> + Clone> Parser<S> for Strg<'a, S> {
    type Target = &'a str;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg>  {
        let mut chars = self.s.chars();
        let mut stream = stream;
        while let Some(ch) = chars.next() {
            stream = char(ch).parse(stream.clone())?.1;
        }
        Ok((self.s, stream))
    }
}

pub fn strg<S>(s: &str) -> Strg<S> {
    Strg::new(s)
}