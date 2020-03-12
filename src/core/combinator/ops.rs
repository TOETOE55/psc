use crate::adaptor::*;
use crate::covert::IntoParser;
use crate::{ParseLogger, Parser, ParserExt};
use std::marker::PhantomData;
use std::ops::{BitOr, Shl, Shr};

/// Alg wrapper
/// 1. `wrap(pa) >> pb <- pa.and_r(pb)`
/// 2. `wrap(pa) << pb <- pa.and_l(pb)`
/// 3. `wrap(pa) | pb <- pa.or(pb)`
pub struct ParserWrapper<S, P> {
    inner: P,
    _marker: PhantomData<fn(&mut S)>,
}

impl<S, P> ParserWrapper<S, P> {
    pub fn new(parser: P) -> Self {
        Self {
            inner: parser,
            _marker: PhantomData,
        }
    }

    pub fn unwrap(self) -> P {
        self.inner
    }
}

impl<S, P: Parser<S>> Parser<S> for ParserWrapper<S, P> {
    type Target = P::Target;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        self.inner.parse(stream, logger)
    }
}

impl<S, P: Clone> Clone for ParserWrapper<S, P> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _marker: PhantomData,
        }
    }
}

impl<S, P: Copy> Copy for ParserWrapper<S, P> {}

impl<S, P: IntoParser<S>> From<P> for ParserWrapper<S, P> {
    fn from(parser: P) -> Self {
        ParserWrapper::new(parser)
    }
}

impl<S, Lhs, Rhs> BitOr<Rhs> for ParserWrapper<S, Lhs>
where
    Lhs: IntoParser<S>,
    Rhs: IntoParser<S, Target = Lhs::Target>,
{
    type Output = ParserWrapper<S, Or<Lhs::Parser, Rhs::Parser>>;
    fn bitor(self, rhs: Rhs) -> Self::Output {
        ParserWrapper::new(self.inner.into_parser().or(rhs))
    }
}

impl<S, Lhs, Rhs> Shl<Rhs> for ParserWrapper<S, Lhs>
where
    Lhs: IntoParser<S>,
    Rhs: IntoParser<S>,
{
    type Output = ParserWrapper<S, AndL<Lhs::Parser, Rhs::Parser>>;

    fn shl(self, rhs: Rhs) -> Self::Output {
        ParserWrapper::new(self.inner.into_parser().and_l(rhs))
    }
}

impl<S, Lhs, Rhs> Shr<Rhs> for ParserWrapper<S, Lhs>
where
    Lhs: IntoParser<S>,
    Rhs: IntoParser<S>,
{
    type Output = ParserWrapper<S, AndR<Lhs::Parser, Rhs::Parser>>;

    fn shr(self, rhs: Rhs) -> Self::Output {
        ParserWrapper::new(self.inner.into_parser().and_r(rhs))
    }
}

pub fn wrap<S, P: IntoParser<S>>(p: P) -> ParserWrapper<S, P::Parser> {
    p.into_parser().into()
}

/// Parse function wrapper
#[derive(Copy, Clone, Debug)]
pub struct ParseFn<F>(pub F);

impl<S, A, F> Parser<S> for ParseFn<F>
where
    F: for<'a> Fn(&'a mut S, &mut ParseLogger) -> Option<A>,
{
    type Target = A;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        (self.0)(stream, logger)
    }
}
