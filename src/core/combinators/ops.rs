use crate::core::combinators::{AndL, AndR, Or};
use crate::covert::IntoParser;
use crate::{ParseMsg, Parser, Stream};
use std::marker::PhantomData;
use std::ops::{BitOr, Shl, Shr};

/// Alg wrapper
/// 1. `pa.wrap() >> pb ~ pa.and_r(pb)`
/// 2. `pa.wrap() << pb ~ pa.and_l(pb)`
/// 3. `pa.wrap() | pb ~ pa.or(pb)`
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

impl<S: Stream, P: IntoParser<S>> From<P> for ParserWrapper<S, P> {
    fn from(parser: P) -> Self {
        ParserWrapper::new(parser)
    }
}

impl<S: Stream, Lhs, Rhs> BitOr<Rhs> for ParserWrapper<S, Lhs>
where
    Lhs: Parser<S>,
    Rhs: IntoParser<S, Target = Lhs::Target>,
{
    type Output = ParserWrapper<S, Or<Lhs, Rhs::Parser>>;
    fn bitor(self, rhs: Rhs) -> Self::Output {
        ParserWrapper::new(self.0.or(rhs))
    }
}

impl<S: Stream, Lhs, Rhs> Shl<Rhs> for ParserWrapper<S, Lhs>
where
    Lhs: Parser<S>,
    Rhs: IntoParser<S>,
{
    type Output = ParserWrapper<S, AndL<Lhs, Rhs::Parser>>;

    fn shl(self, rhs: Rhs) -> Self::Output {
        ParserWrapper::new(self.0.and_l(rhs))
    }
}

impl<S: Stream, Lhs, Rhs> Shr<Rhs> for ParserWrapper<S, Lhs>
where
    Lhs: Parser<S>,
    Rhs: IntoParser<S>,
{
    type Output = ParserWrapper<S, AndR<Lhs, Rhs::Parser>>;

    fn shr(self, rhs: Rhs) -> Self::Output {
        ParserWrapper::new(self.0.and_r(rhs))
    }
}

/// Parse function wrapper
/// ```
/// use psc::{Stream, ParseMsg, satisfy, ParseFn, Parser, pure};
/// fn parse_fn<S: Stream<Item = char> + Clone>(stream: &mut S) -> Result<(), ParseMsg> {
///        let parser =
///         (satisfy(|ch: &char| ch.is_uppercase()).wrap() >> ParseFn(parse_fn))
///        | satisfy(|ch: &char| ch.is_lowercase()).wrap() >> pure(|| {});
///        parser.parse(stream)
///    }
/// ```
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
