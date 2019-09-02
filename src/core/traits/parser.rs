use crate::core::traits::stream::Stream;
use std::rc::Rc;
use crate::core::combinators::{Or, And, App, Map, AndThen, Many, Some};

pub trait Parser<S: Stream> {
    type Target;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)>;

    fn or<P>(self, other: P) -> Or<Self, P> where
        Self: Sized,
        P: Parser<S, Target=Self::Target>,
    {
        Or::new(self, other)
    }

    fn and<P>(self, other: P) -> And<Self, P> where
        Self: Sized,
        P: Parser<S, Target=Self::Target>,
    {
        And::new(self, other)
    }

    fn app<PF, T, F>(self, pf: PF) -> App<Self, PF> where
        Self: Sized,
        F: Fn(Self::Target) -> T,
        PF: Parser<S, Target=F>,
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
        P: Parser<S>,
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
}

impl<S: Stream, P: Parser<S> + ?Sized> Parser<S> for &P {
    type Target = P::Target;
    fn parse(&self, state: S) -> Option<(Self::Target, S)> {
        (**self).parse(state)
    }
}

impl<S: Stream, P: Parser<S> + ?Sized> Parser<S> for &mut P {
    type Target = P::Target;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)> {
        (**self).parse(stream)
    }
}

impl<S: Stream, P: Parser<S> + ?Sized> Parser<S> for Box<P> {
    type Target = P::Target;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)> {
        (**self).parse(stream)
    }
}

impl<S: Stream, P: Parser<S> + ?Sized> Parser<S> for Rc<P> {
    type Target = P::Target;
    fn parse(&self, stream: S) -> Option<(Self::Target, S)> {
        (**self).parse(stream)
    }
}
