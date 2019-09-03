use crate::core::traits::stream::Stream;
use crate::core::combinators::{Or, And, App, Map, AndThen, Many, Some, Failure, failure};
use crate::core::err::ParseMsg;
use std::marker::PhantomData;
use std::rc::Rc;

pub trait Parser {
    type Stream: Stream;
    type Target;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg>;

    fn or<P>(self, other: P) -> Or<Self, P> where
        Self: Sized,
        P: Parser<Stream=Self::Stream, Target=Self::Target>,
    {
        Or::new(self, other)
    }

    fn and<P>(self, other: P) -> And<Self, P> where
        Self: Sized,
        P: Parser<Stream=Self::Stream, Target=Self::Target>,
    {
        And::new(self, other)
    }

    fn app<PF, T, F>(self, pf: PF) -> App<Self, PF> where
        Self: Sized,
        F: Fn(Self::Target) -> T,
        PF: Parser<Stream=Self::Stream, Target=F>,
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
        P: Parser<Stream=Self::Stream>,
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

    fn label(self, msg: String) -> Or<Self, Failure<Self::Stream, Self::Target>> where Self: Sized {
        self.or(failure(ParseMsg::Except(msg)))
    }
}

impl<P: Parser + ?Sized> Parser for &P {
    type Stream = P::Stream;
    type Target = P::Target;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg> {
        (**self).parse(stream)
    }
}

impl<P: Parser + ?Sized> Parser for &mut P {
    type Stream = P::Stream;
    type Target = P::Target;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg> {
        (**self).parse(stream)
    }
}


impl<P: Parser + ?Sized> Parser for Box<P> {
    type Stream = P::Stream;
    type Target = P::Target;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg> {
        (**self).parse(stream)
    }
}


impl<P: Parser + ?Sized> Parser for Rc<P> {
    type Stream = P::Stream;
    type Target = P::Target;
    fn parse(&self, stream: Self::Stream) -> Result<(Self::Target, Self::Stream), ParseMsg> {
        (**self).parse(stream)
    }
}

