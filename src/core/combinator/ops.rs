use crate::adaptor::{AndL, AndR, Or};
use crate::core::traits::covert::IntoParser;
use crate::{Consumed, ParseResult, Parser};
use std::marker::PhantomData;
use std::ops::{BitOr, Shl, Shr};

#[derive(Copy, Clone, Debug)]
pub struct ParseFn<F>(pub F);

impl<S, E, A, F> Parser<S, E> for ParseFn<F>
where
    F: Fn(&mut S, &mut E) -> ParseResult<A>,
{
    type Target = A;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        (self.0)(s, err)
    }
}

pub struct ParserWrapper<S, E, P> {
    inner: P,
    _marker: PhantomData<fn(&mut S, &mut E)>,
}

impl<S, E, P> ParserWrapper<S, E, P> {
    pub fn new(inner: P) -> Self {
        Self {
            inner,
            _marker: PhantomData,
        }
    }

    pub fn unwrap(self) -> P {
        self.inner
    }
}

/**
# Parser Operator
## Example
```
use psc::*;

let parser = (wrap('p') >> "pq") | "pq"; // char('p').and_r("pq").or("pq")
```
*/
pub fn wrap<S, E, P>(p: P) -> ParserWrapper<S, E, P::Parser>
where
    P: IntoParser<S, E>,
{
    ParserWrapper::new(p.into_parser())
}

impl<S, E, P: Parser<S, E>> Parser<S, E> for ParserWrapper<S, E, P> {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        self.inner.parse(s, err)
    }
}

impl<S, E, P, Q> BitOr<Q> for ParserWrapper<S, E, P>
where
    P: IntoParser<S, E>,
    Q: IntoParser<S, E, Target = P::Target>,
{
    type Output = ParserWrapper<S, E, Or<P::Parser, Q::Parser>>;

    fn bitor(self, rhs: Q) -> Self::Output {
        ParserWrapper::new(Or::new(self.inner.into_parser(), rhs.into_parser()))
    }
}

impl<S, E, P, Q> Shl<Q> for ParserWrapper<S, E, P>
where
    P: IntoParser<S, E>,
    Q: IntoParser<S, E>,
{
    type Output = ParserWrapper<S, E, AndL<P::Parser, Q::Parser>>;

    fn shl(self, rhs: Q) -> Self::Output {
        ParserWrapper::new(AndL::new(self.inner.into_parser(), rhs.into_parser()))
    }
}

impl<S, E, P, Q> Shr<Q> for ParserWrapper<S, E, P>
where
    P: IntoParser<S, E>,
    Q: IntoParser<S, E>,
{
    type Output = ParserWrapper<S, E, AndR<P::Parser, Q::Parser>>;

    fn shr(self, rhs: Q) -> Self::Output {
        ParserWrapper::new(AndR::new(self.inner.into_parser(), rhs.into_parser()))
    }
}
