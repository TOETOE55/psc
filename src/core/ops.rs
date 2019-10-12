use crate::core::combinators::{AndL, AndR, Or};
use crate::{ParseMsg, Parser, Stream};
use std::marker::PhantomData;
use std::ops::{BitOr, Shl, Shr};

pub struct ParserWrapper<S, P>(P, PhantomData<S>);

impl<S, P> ParserWrapper<S, P> {
    pub fn new(parser: P) -> Self {
        Self(parser, PhantomData)
    }

    pub fn unwrap(self) -> P {
        self.0
    }
}

impl<S: Stream, P: Parser<S>> Parser<S> for ParserWrapper<S, P> {
    type Target = P::Target;

    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        self.0.parse(stream)
    }
}

impl<S: Stream, P: Parser<S>> From<P> for ParserWrapper<S, P> {
    fn from(parser: P) -> Self {
        ParserWrapper::new(parser)
    }
}

impl<S: Stream, P, Q> BitOr<P> for ParserWrapper<S, Q>
where
    P: Parser<S>,
    Q: Parser<S, Target = P::Target>,
{
    type Output = ParserWrapper<S, Or<Q, P>>;
    fn bitor(self, rhs: P) -> Self::Output {
        ParserWrapper::new(self.0.or(rhs))
    }
}

impl<S: Stream, P, Q> Shl<P> for ParserWrapper<S, Q>
where
    P: Parser<S>,
    Q: Parser<S>,
{
    type Output = ParserWrapper<S, AndL<Q, P>>;

    fn shl(self, rhs: P) -> Self::Output {
        ParserWrapper::new(self.0.and_l(rhs))
    }
}

impl<S: Stream, P, Q> Shr<P> for ParserWrapper<S, Q>
where
    P: Parser<S>,
    Q: Parser<S>,
{
    type Output = ParserWrapper<S, AndR<Q, P>>;

    fn shr(self, rhs: P) -> Self::Output {
        ParserWrapper::new(self.0.and_r(rhs))
    }
}

pub struct ParseFn<F>(pub F);

impl<F> ParseFn<F> {
    pub fn call<A, B>(&self, arg: A) -> B
    where
        F: Fn(A) -> B,
    {
        (self.0)(arg)
    }
}

impl<S: Stream, A, F> Parser<S> for ParseFn<F>
where
    F: for<'a> Fn(&'a mut S) -> Result<A, ParseMsg>,
{
    type Target = A;

    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        self.call(stream)
    }
}
