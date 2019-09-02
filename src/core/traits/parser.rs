use crate::core::traits::stream::Stream;
use std::rc::Rc;
use crate::core::combinators::{Or, And, App, Map, AndThen, Many, Some, Failure, failure};

pub trait Parser<S: Stream, E: Clone> {
    type Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), E>;

    fn or<P>(self, other: P) -> Or<Self, P> where
        Self: Sized,
        P: Parser<S, E, Target=Self::Target>,
    {
        Or::new(self, other)
    }

    fn and<P>(self, other: P) -> And<Self, P> where
        Self: Sized,
        P: Parser<S, E, Target=Self::Target>,
    {
        And::new(self, other)
    }

    fn app<PF, T, F>(self, pf: PF) -> App<Self, PF> where
        Self: Sized,
        F: Fn(Self::Target) -> T,
        PF: Parser<S, E, Target=F>,
    {
        App::new(self, pf)
    }

    fn map<B, F>(self, f: F) -> Map<Self, F> where
        Self: Sized,
        F: Fn(Self::Target) -> B,
    {
        Map::new(self, f)
    }

    fn and_then<P, F>(self, f: F) -> AndThen<Self, F> where
        Self: Sized,
        P: Parser<S, E>,
        F: Fn(Self::Target) -> P,
    {
        AndThen::new(self, f)
    }

    fn many(self) -> Many<Self> where Self: Sized {
        Many::new(self)
    }

    fn some(self) -> Some<Self> where Self: Sized {
        Some::new(self)
    }

    fn with_err(self, msg: E) -> Or<Self, Failure<Self::Target, E>> where Self: Sized {
        self.or(failure(msg))
    }
}

impl<S: Stream, E: Clone, P: Parser<S, E> + ?Sized> Parser<S, E> for &P {
    type Target = P::Target;
    fn parse(&self, state: S) -> Result<(Self::Target, S), E> {
        (**self).parse(state)
    }
}

impl<S: Stream, E: Clone, P: Parser<S, E> + ?Sized> Parser<S, E> for &mut P {
    type Target = P::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), E> {
        (**self).parse(stream)
    }
}

impl<S: Stream, E: Clone, P: Parser<S, E> + ?Sized> Parser<S, E> for Box<P> {
    type Target = P::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), E> {
        (**self).parse(stream)
    }
}

impl<S: Stream, E: Clone, P: Parser<S, E> + ?Sized> Parser<S, E> for Rc<P> {
    type Target = P::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), E> {
        (**self).parse(stream)
    }
}
