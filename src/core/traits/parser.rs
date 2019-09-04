use crate::core::traits::stream::Stream;
use crate::core::combinators::{Or, AndR, App, Map, AndThen, Many, Some, Failure, AndL, ChainL, ChainR, Join};
use crate::core::err::ParseMsg;
use std::rc::Rc;

pub trait Parser<S: Stream> {
    type Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg>;

    fn or<P>(self, other: P) -> Or<Self, P> where
        Self: Sized,
        P: Parser<S, Target=Self::Target>,
    {
        Or::new(self, other)
    }

    fn and_r<P>(self, other: P) -> AndR<Self, P> where
        Self: Sized,
        P: Parser<S>,
    {
        AndR::new(self, other)
    }

    fn and_l<P>(self, other: P) -> AndL<Self, P> where
        Self: Sized,
        P: Parser<S, Target=Self::Target>,
    {
        AndL::new(self, other)
    }

    fn map<B, F>(self, f: F) -> Map<Self, F> where
        Self: Sized,
        F: Fn(Self::Target) -> B,
    {
        Map::new(self, f)
    }

    fn app<PF, T, F>(self, pf: PF) -> App<Self, PF> where
        Self: Sized,
        F: Fn(Self::Target) -> T,
        PF: Parser<S, Target=F>,
    {
        App::new(self, pf)
    }

    fn and_then<P, F>(self, f: F) -> AndThen<Self, F> where
        Self: Sized,
        P: Parser<S>,
        F: Fn(Self::Target) -> P,
    {
        AndThen::new(self, f)
    }

    fn chain_l<P>(self, other: P) -> ChainL<Self, P> where
        Self: Sized,
        P: Parser<S, Target=Vec<Self::Target>>,
    {
        ChainL::new(self, other)
    }

    fn chain_r<P>(self, other: P) -> ChainR<Self, P> where
        Self: Parser<S, Target=Vec<P::Target>> + Sized,
        P: Parser<S>,
    {
        ChainR::new(self, other)
    }

    fn many(self) -> Many<Self> where Self: Sized {
        Many::new(self)
    }

    fn some(self) -> Some<Self> where Self: Sized {
        Some::new(self)
    }

    fn label(self, msg: String) -> Or<Self, Failure<S, Self::Target>> where Self: Sized {
        self.or(Failure::new(ParseMsg::Except(msg)))
    }

    fn join(self) -> Join<Self> where
        Self: Sized,
        Self::Target: Parser<S>,
    {
        Join::new(self)
    }
}

impl<S: Stream, P: Parser<S> + ?Sized> Parser<S> for &P {
    type Target = P::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        (**self).parse(stream)
    }
}

impl<S: Stream, P: Parser<S> + ?Sized> Parser<S> for &mut P {
    type Target = P::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        (**self).parse(stream)
    }
}

impl<S: Stream, P: Parser<S> + ?Sized> Parser<S> for Box<P> {
    type Target = P::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        (**self).parse(stream)
    }
}

impl<S: Stream, P: Parser<S> + ?Sized> Parser<S> for Rc<P> {
    type Target = P::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        (**self).parse(stream)
    }
}

