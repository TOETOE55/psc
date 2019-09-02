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
impl<S: Stream, T, F: Fn() -> T> Parser<S> for Pure<F> {
    type Target = T;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)> {
        Some(((self.x)(), stream))
    }
}

pub fn pure<T, F: Fn() -> T>(x: F) -> Pure<F> {
    Pure { x }
}

/// failure
pub struct Failure<T>(PhantomData<T>);
impl<S: Stream, T> Parser<S> for Failure<T> {
    type Target = T;
    fn parse(&self, _stream: S) -> Option<(Self::Target, S)> {
        None
    }
}

pub fn failure<T>() -> Failure<T> {
    Failure(PhantomData)
}

/// fix
pub struct Fix<S: Stream, A> {
    fix: Rc<dyn Fn(Fix<S, A>) -> Box<dyn Parser<S, Target=A>>>
}

impl<S: Stream, A> Clone for Fix<S, A> {
    fn clone(&self) -> Self {
        Fix {
            fix: self.fix.clone(),
        }
    }
}

impl<S: Stream, A> Parser<S> for Fix<S, A> {
    type Target = A;
    fn parse(&self, state: S) -> Option<(Self::Target, S)> {
        (self.fix)((*self).clone()).parse(state)
    }
}

pub fn fix<S: Stream, A>(fix: Rc<dyn Fn(Fix<S, A>) -> Box<dyn Parser<S, Target=A>>>) -> Fix<S, A> {
    Fix { fix }
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

impl<S: Stream + Clone, A, B> Parser<S> for Or<A, B> where
    A: Parser<S>,
    B: Parser<S, Target=A::Target>,
{
    type Target = A::Target;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)> {
        self.a.parse(stream.clone()).or_else(|| self.b.parse(stream))
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

impl<S: Stream, A: Parser<S>, B: Parser<S>> Parser<S> for And<A, B> {
    type Target = B::Target;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)> {
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

impl<S: Stream, B, P: Parser<S>, F> Parser<S> for Map<P, F>
    where F: Fn(P::Target) -> B,
{
    type Target = B;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)> {
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

impl<S: Stream, A, AB, T, F> Parser<S> for App<A, AB> where
    A: Parser<S>,
    F: Fn(A::Target) -> T,
    AB: Parser<S, Target=F>,
{
    type Target = T;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)> {
        self.ab.parse(stream)
            .and_then(|(f, stream)| self.a.parse(stream)
                .and_then(|(a, stream)| Some((f(a), stream))))
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

impl<S: Stream, A, B, F> Parser<S> for AndThen<A, F> where
    A: Parser<S>,
    B: Parser<S>,
    F: Fn(A::Target) -> B,
{
    type Target = B::Target;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)> {
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

impl<S: Stream + Clone, P: Parser<S>> Parser<S> for Many<P> {
    type Target = Vec<P::Target>;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)> {
        let mut vec = vec![];
        let mut stream = stream;
        loop {
            match self.parser.parse(stream.clone()) {
                Some((a, s)) => {
                    vec.push(a);
                    stream = s;
                },
                _ => break,
            }
        }

        Some((vec, stream))
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

impl<S: Stream + Clone, P: Parser<S>> Parser<S> for Some<P> {
    type Target = Vec<P::Target>;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)> {
        let mut vec = vec![];
        let mut stream = stream;

        self.parser.parse(stream.clone()).map(|(a, s)| {
            vec.push(a);
            stream = s;
        })?;

        loop {
            match self.parser.parse(stream.clone()) {
                Some((a, s)) => {
                    vec.push(a);
                    stream = s;
                },
                _ => break,
            }
        }

        Some((vec, stream))
    }
}
