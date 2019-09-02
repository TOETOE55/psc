pub mod common;

use std::marker::PhantomData;
use std::rc::Rc;
use crate::core::traits::parser::Parser;
use crate::core::traits::stream::Stream;

///
#[derive(Clone)]
pub struct Pure<F> {
    x: F,
}

impl<F> Pure<F> {
    pub fn new(x: F) -> Self {
        Self { x }
    }
}

impl<S: Stream, E: Clone, T, F: Fn() -> T> Parser<S, E> for Pure<F> {
    type Target = T;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), E> {
        Ok(((self.x)(), stream))
    }
}

pub fn pure<T, F: Fn() -> T>(x: F) -> Pure<F> {
    Pure::new(x)
}

/// failure
pub struct Failure<T, E>{
    msg: E,
    _p: PhantomData<T>,
}

impl<T, E> Failure<T, E> {
    pub fn new(msg: E) -> Self {
        Self {
            msg,
            _p: PhantomData,
        }
    }
}
impl<S: Stream, E: Clone, T> Parser<S, E> for Failure<T, E> {
    type Target = T;
    fn parse(&self, _stream: S) -> Result<(Self::Target, S), E> {
        Err(self.msg.clone())
    }
}

pub fn failure<T, E: Clone>(msg: E) -> Failure<T, E> {
    Failure::new(msg)
}

/// fix
pub struct Fix<S: Stream, E, A> {
    fix: Rc<dyn Fn(Fix<S, E, A>) -> Box<dyn Parser<S, E, Target=A>>>
}

impl<S: Stream, E: Clone, A> Fix<S, E, A> {
    pub fn new(fix: Rc<dyn Fn(Fix<S, E, A>) -> Box<dyn Parser<S, E, Target=A>>>) -> Self {
        Self { fix }
    }
}

impl<S: Stream, E: Clone, A> Clone for Fix<S, E, A> {
    fn clone(&self) -> Self {
        Fix {
            fix: self.fix.clone(),
        }
    }
}

impl<S: Stream, E: Clone, A> Parser<S, E> for Fix<S, E, A> {
    type Target = A;
    fn parse(&self, state: S) -> Result<(Self::Target, S), E> {
        (self.fix)((*self).clone()).parse(state)
    }
}

pub fn fix<S: Stream, E: Clone, A>(fix: Rc<dyn Fn(Fix<S, E, A>)
    -> Box<dyn Parser<S, E, Target=A>>>) -> Fix<S, E, A> {
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

impl<S: Stream + Clone, E: Clone, A, B> Parser<S, E> for Or<A, B> where
    A: Parser<S, E>,
    B: Parser<S, E, Target=A::Target>,
{
    type Target = A::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), E> {
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

impl<S: Stream, E: Clone, A: Parser<S, E>, B: Parser<S, E>> Parser<S, E> for And<A, B> {
    type Target = B::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), E> {
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

impl<S: Stream, E: Clone, B, P: Parser<S, E>, F> Parser<S, E> for Map<P, F>
    where F: Fn(P::Target) -> B,
{
    type Target = B;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), E> {
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

impl<S: Stream, E: Clone, A, AB, T, F> Parser<S, E> for App<A, AB> where
    A: Parser<S, E>,
    F: Fn(A::Target) -> T,
    AB: Parser<S, E, Target=F>,
{
    type Target = T;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), E> {
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

impl<S: Stream, E: Clone, A, B, F> Parser<S, E> for AndThen<A, F> where
    A: Parser<S, E>,
    B: Parser<S, E>,
    F: Fn(A::Target) -> B,
{
    type Target = B::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), E> {
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

impl<S: Stream + Clone, E: Clone, P: Parser<S, E>> Parser<S, E> for Many<P> {
    type Target = Vec<P::Target>;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), E> {
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

impl<S: Stream + Clone, E: Clone, P: Parser<S, E>> Parser<S, E> for Some<P> {
    type Target = Vec<P::Target>;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), E> {
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
