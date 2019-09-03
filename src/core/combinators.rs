pub mod common;

use std::marker::PhantomData;
use std::rc::Rc;
use crate::core::traits::parser::Parser;
use crate::core::traits::stream::Stream;
use crate::core::err::ParseMsg;

///
#[derive(Clone)]
pub struct Pure<S, F> {
    x: F,
    _s: PhantomData<S>,
}

impl<S, F> Pure<S, F> {
    pub fn new(x: F) -> Self {
        Self {
            x,
            _s: PhantomData,
        }
    }
}

impl<S: Stream, T, F: Fn() -> T> Parser for Pure<S, F> {
    type Stream = S;
    type Target = T;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg>  {
        Ok(((self.x)(), stream))
    }
}

pub fn pure<S, T, F: Fn() -> T>(x: F) -> Pure<S, F> {
    Pure::new(x)
}

/// failure
pub struct Failure<S, T>{
    msg: ParseMsg,
    _p: PhantomData<T>,
    _s: PhantomData<S>,
}

impl<S, T> Failure<S, T> {
    pub fn new(msg: ParseMsg) -> Self {
        Self {
            msg,
            _p: PhantomData,
            _s: PhantomData,
        }
    }
}
impl<S: Stream, T> Parser for Failure<S, T> {
    type Stream = S;
    type Target = T;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg>  {
        Err(self.msg.clone())
    }
}

pub fn failure<S, T>(msg: ParseMsg) -> Failure<S, T> {
    Failure::new(msg)
}

/// fix
pub struct Fix<S: Stream, A> {
    fix: Rc<dyn Fn(Fix<S, A>) -> Box<dyn Parser<Stream=S, Target=A>>>
}

impl<S: Stream, A> Fix<S, A> {
    pub fn new(fix: Rc<dyn Fn(Fix<S, A>) -> Box<dyn Parser<Stream=S, Target=A>>>) -> Self {
        Self { fix }
    }
}

impl<S: Stream, A> Clone for Fix<S, A> {
    fn clone(&self) -> Self {
        Fix {
            fix: self.fix.clone(),
        }
    }
}

impl<S: Stream, A> Parser for Fix<S, A> {
    type Stream = S;
    type Target = A;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        (self.fix)((*self).clone()).parse(stream)
    }
}

pub fn fix<S: Stream, A>(fix: Rc<dyn Fn(Fix<S, A>)
    -> Box<dyn Parser<Stream=S, Target=A>>>) -> Fix<S, A> {
    Fix::new(fix)
}

/// or
#[derive(Clone)]
pub struct Or<A, B> {
    a: A,
    b: B,
}

impl<A, B> Or<A, B> {
    pub fn new(a:A, b: B) -> Self {
        Self { a, b }
    }
}

impl<A, B> Parser for Or<A, B> where
    A: Parser,
    B: Parser<Stream=A::Stream, Target=A::Target>,
{
    type Stream = A::Stream;
    type Target = A::Target;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg>  {
        self.a.parse(stream.clone()).or_else(|_| self.b.parse(stream))
    }
}

/// and
#[derive(Clone)]
pub struct And<A, B> {
    a: A,
    b: B,
}

impl<A, B> And<A, B> {
    pub fn new(a:A, b: B) -> Self {
        Self { a, b }
    }
}

impl<A: Parser, B: Parser<Stream=A::Stream>> Parser for And<A, B> {
    type Stream = A::Stream;
    type Target = B::Target;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg>  {
        self.a.parse(stream).and_then(|(_, stream)| self.b.parse(stream))
    }
}

/// map
#[derive(Clone)]
pub struct Map<P, F> {
    parser: P,
    f: F,
}

impl<P, F> Map<P, F> {
    pub fn new(parser: P, f: F) -> Self {
        Self { parser, f }
    }
}

impl<B, P: Parser, F> Parser for Map<P, F>
    where F: Fn(P::Target) -> B,
{
    type Stream = P::Stream;
    type Target = B;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg>  {
        self.parser.parse(stream)
            .map(|(res, stream)| ((self.f)(res), stream))
    }
}

/// applicative
#[derive(Clone)]
pub struct App<A, AB> {
    a: A,
    ab: AB,
}

impl<A, AB> App<A, AB> {
    pub fn new(a: A, ab: AB) -> Self {
        Self { a, ab }
    }
}

impl<A, AB, T, F> Parser for App<A, AB> where
    A: Parser,
    F: Fn(A::Target) -> T,
    AB: Parser<Stream=A::Stream, Target=F>,
{
    type Stream = A::Stream;
    type Target = T;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg>  {
        self.ab.parse(stream)
            .and_then(|(f, stream)| self.a.parse(stream)
                .and_then(|(a, stream)| Ok((f(a), stream))))
    }
}

/// and_then
#[derive(Clone)]
pub struct AndThen<P, F> {
    parser: P,
    f: F,
}

impl<P, F> AndThen<P, F> {
    pub fn new(parser: P, f: F) -> Self {
        Self { parser, f }
    }
}

impl<A, B, F> Parser for AndThen<A, F> where
    A: Parser,
    B: Parser<Stream=A::Stream>,
    F: Fn(A::Target) -> B,
{
    type Stream = A::Stream;
    type Target = B::Target;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg>  {
        self.parser.parse(stream)
            .and_then(|(a, stream)| (self.f)(a).parse(stream))

    }
}

/// many
pub struct Many<P> {
    parser: P,
}

impl<P> Many<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<P: Parser> Parser for Many<P> {
    type Stream = P::Stream;
    type Target = Vec<P::Target>;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg>  {
        let mut vec = vec![];
        let mut stream = stream;
        loop {
            match self.parser.parse(stream.clone()) {
                Ok((a, s)) => {
                    vec.push(a);
                    stream = s;
                },
                _ => break,
            }
        }

        Ok((vec, stream))
    }
}


/// some
#[derive(Clone)]
pub struct Some<P> {
    parser: P,
}

impl<P> Some<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<P: Parser> Parser for Some<P> {
    type Stream = P::Stream;
    type Target = Vec<P::Target>;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg> {
        let mut vec = vec![];
        let mut stream = stream;

        self.parser.parse(stream.clone()).map(|(a, s)| {
            vec.push(a);
            stream = s;
        })?;

        loop {
            match self.parser.parse(stream.clone()) {
                Ok((a, s)) => {
                    vec.push(a);
                    stream = s;
                },
                _ => break,
            }
        }

        Ok((vec, stream))
    }
}
