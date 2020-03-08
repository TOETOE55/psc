use crate::core::traits::err::{ParseErr, ParseLogger};
use std::fmt::Debug;
use std::marker::PhantomData;

pub trait Parser<S, E = ParseErr> {
    type Target;
    fn parse(&self, s: &mut S, err: &mut E) -> ParseResult<Self::Target>;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Consumed<A> {
    Some(A),
    Empty(A),
}

pub type ParseResult<A> = Consumed<Option<A>>;

pub trait ParserExt<S, E>: Parser<S, E> {}

impl<S, E, P: Parser<S, E>> ParserExt<S, E> for P {}

pub trait IntoParser<S, E> {
    type Target;
    type Parser: Parser<S, E, Target = Self::Target>;
    fn into_parser(self) -> Self::Parser;
}

impl<S, E, P: Parser<S, E>> IntoParser<S, E> for P {
    type Target = P::Target;
    type Parser = P;

    fn into_parser(self) -> Self::Parser {
        self
    }
}

impl<A> Consumed<A> {
    pub fn value(self) -> A {
        match self {
            Consumed::Some(x) => x,
            Consumed::Empty(x) => x,
        }
    }
}

impl<A> Consumed<Option<A>> {
    pub fn map<B, F>(self, f: F) -> Consumed<Option<B>>
    where
        F: FnOnce(A) -> B,
    {
        match self {
            Consumed::Some(x) => Consumed::Some(x.map(f)),
            Consumed::Empty(x) => Consumed::Empty(x.map(f)),
        }
    }

    pub fn flat_map<B, F>(self, f: F) -> Consumed<Option<B>>
    where
        F: FnOnce(A) -> Consumed<Option<B>>,
    {
        match self {
            Consumed::Some(Some(x)) | Consumed::Empty(Some(x)) => f(x),
            Consumed::Some(None) => Consumed::Some(None),
            Consumed::Empty(None) => Consumed::Empty(None),
        }
    }
}

impl<A: std::ops::Try> std::ops::Try for Consumed<A> {
    type Ok = Consumed<A::Ok>;
    type Error = Consumed<A::Error>;

    fn into_result(self) -> Result<Self::Ok, Self::Error> {
        match self {
            Consumed::Some(x) => x.into_result().map(Consumed::Some).map_err(Consumed::Some),
            Consumed::Empty(x) => x
                .into_result()
                .map(Consumed::Empty)
                .map_err(Consumed::Empty),
        }
    }

    fn from_error(v: Self::Error) -> Self {
        match v {
            Consumed::Some(v) => Consumed::Some(std::ops::Try::from_error(v)),
            Consumed::Empty(v) => Consumed::Empty(std::ops::Try::from_error(v)),
        }
    }

    fn from_ok(v: Self::Ok) -> Self {
        match v {
            Consumed::Some(v) => Consumed::Some(std::ops::Try::from_ok(v)),
            Consumed::Empty(v) => Consumed::Empty(std::ops::Try::from_ok(v)),
        }
    }
}

/**
*/
#[derive(Debug)]
pub struct Empty<S, E, A> {
    _marker: PhantomData<fn(&mut S, &mut E) -> A>,
}

impl<A, S, E> Empty<S, E, A> {
    pub fn new() -> Self {
        Self {
            _marker: PhantomData,
        }
    }
}

impl<A, S, E> Default for Empty<S, E, A> {
    fn default() -> Self {
        Self::new()
    }
}

impl<A, S, E> Clone for Empty<S, E, A> {
    fn clone(&self) -> Self {
        Self::new()
    }
}

impl<A, S, E> Copy for Empty<S, E, A> {}

impl<A, S, E> Parser<S, E> for Empty<S, E, A> {
    type Target = A;

    fn parse(&self, _: &mut S, _: &mut E) -> ParseResult<Self::Target> {
        Consumed::Empty(None)
    }
}

#[derive(Debug)]
pub struct Pure<S, E, F> {
    inner: F,
    _marker: PhantomData<fn(&mut S, &mut E) -> F>,
}

impl<S, E, F> Pure<S, E, F> {
    pub fn new(x: F) -> Self {
        Self {
            inner: x,
            _marker: PhantomData,
        }
    }
}

/**
*/
impl<S, E, F: Clone> Clone for Pure<S, E, F> {
    fn clone(&self) -> Self {
        Self::new(self.inner.clone())
    }
}

impl<S, E, F: Copy> Copy for Pure<S, E, F> {}

impl<S, E, A, F: Fn() -> A> Parser<S, E> for Pure<S, E, F> {
    type Target = A;

    fn parse(&self, _: &mut S, _: &mut E) -> Consumed<Option<Self::Target>> {
        Consumed::Empty(Some((self.inner)()))
    }
}

/**
*/
#[derive(Debug, Clone)]
pub struct Fail<S, E, A> {
    msg: String,
    _marker: PhantomData<fn(&mut S, &mut E) -> A>,
}

impl<S, E, A> Fail<S, E, A> {
    pub fn new(msg: &str) -> Self {
        Self {
            msg: msg.to_string(),
            _marker: PhantomData,
        }
    }
}

impl<S, E: ParseLogger, A> Parser<S, E> for Fail<S, E, A> {
    type Target = A;

    fn parse(&self, _: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        err.clear();
        err.err(&self.msg);
        Consumed::Empty(None)
    }
}

/**
*/
#[derive(Clone, Copy)]
pub struct Map<P, F> {
    parser: P,
    f: F,
}

impl<P, F> Map<P, F> {
    pub fn new(parser: P, f: F) -> Self {
        Self { parser, f }
    }
}

impl<S, E, P, A, F> Parser<S, E> for Map<P, F>
where
    P: Parser<S, E>,
    F: Fn(P::Target) -> A,
{
    type Target = A;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        self.parser.parse(s, err).map(|t| (self.f)(t))
    }
}

/**
*/
#[derive(Clone, Copy)]
pub struct Map2<P1, P2, F> {
    p1: P1,
    p2: P2,
    f: F,
}

impl<P1, P2, F> Map2<P1, P2, F> {
    pub fn new(p1: P1, p2: P2, f: F) -> Self {
        Self { p1, p2, f }
    }
}

impl<S, E, P1, P2, A, F> Parser<S, E> for Map2<P1, P2, F>
where
    P1: Parser<S, E>,
    P2: Parser<S, E>,
    F: Fn(P1::Target, P2::Target) -> A,
{
    type Target = A;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        self.p1
            .parse(s, err)
            .flat_map(|a| self.p2.parse(s, err).map(|b| (self.f)(a, b)))
    }
}

/**
*/
#[derive(Clone, Copy)]
pub struct AndThen<P, F> {
    parser: P,
    f: F,
}

impl<P, F> AndThen<P, F> {
    pub fn new(parser: P, f: F) -> Self {
        Self { parser, f }
    }
}

impl<S, E, P, TP, F> Parser<S, E> for AndThen<P, F>
where
    P: Parser<S, E>,
    TP: IntoParser<S, E>,
    F: Fn(P::Target) -> TP,
{
    type Target = TP::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        self.parser
            .parse(s, err)
            .flat_map(|x| (self.f)(x).into_parser().parse(s, err))
    }
}

/**
*/
#[derive(Copy, Clone, Debug)]
pub struct Or<PA, PB> {
    pa: PA,
    pb: PB,
}

impl<PA, PB> Or<PA, PB> {
    pub fn new(pa: PA, pb: PB) -> Self {
        Self { pa, pb }
    }
}

impl<S, E, PA, PB> Parser<S, E> for Or<PA, PB>
where
    S: Clone,
    E: Clone,
    PA: Parser<S, E>,
    PB: Parser<S, E, Target = PA::Target>,
{
    type Target = PA::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        let s0 = s.clone();
        let err0 = err.clone();
        match self.pa.parse(s, err) {
            Consumed::Empty(Some(x)) => {
                let s1 = s.clone();
                let err1 = err.clone();
                *s = s0;
                *err = err0;
                match self.pb.parse(s, err) {
                    Consumed::Empty(_) => {
                        *s = s1;
                        *err = err1;
                        Consumed::Empty(Some(x))
                    }
                    consumed => consumed,
                }
            }
            Consumed::Empty(None) => {
                *s = s0;
                *err = err0;
                self.pb.parse(s, err)
            }
            consumed => consumed,
        }
    }
}

/**
*/
pub struct Attempt<P> {
    parser: P,
}

impl<P> Attempt<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S, E, P: Parser<S, E>> Parser<S, E> for Attempt<P> {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        match self.parser.parse(s, err) {
            Consumed::Some(None) => Consumed::Empty(None),
            others => others,
        }
    }
}

/**
*/
#[derive(Clone, Debug)]
pub struct AndR<PA, PB> {
    pa: PA,
    pb: PB,
}

impl<PA, PB> AndR<PA, PB> {
    pub fn new(pa: PA, pb: PB) -> Self {
        Self { pa, pb }
    }
}

impl<S, E, PA, PB> Parser<S, E> for AndR<PA, PB>
where
    PA: Parser<S, E>,
    PB: Parser<S, E>,
{
    type Target = PB::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        self.pa.parse(s, err)?;
        self.pb.parse(s, err)
    }
}

/**
*/
#[derive(Clone, Debug)]
pub struct AndL<PA, PB> {
    pa: PA,
    pb: PB,
}

impl<PA, PB> AndL<PA, PB> {
    pub fn new(pa: PA, pb: PB) -> Self {
        Self { pa, pb }
    }
}

impl<S, E, PA, PB> Parser<S, E> for AndL<PA, PB>
where
    PA: Parser<S, E>,
    PB: Parser<S, E>,
{
    type Target = PA::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        let a = self.pa.parse(s, err)?.value();
        self.pb.parse(s, err).map(|_| a)
    }
}

/**
*/
pub struct Flatten<PP> {
    pp: PP,
}

impl<PP> Flatten<PP> {
    pub fn new(pp: PP) -> Self {
        Self { pp }
    }
}

impl<S, E, PP> Parser<S, E> for Flatten<PP>
where
    PP: Parser<S, E>,
    PP::Target: IntoParser<S, E>,
{
    type Target = <<PP as Parser<S, E>>::Target as IntoParser<S, E>>::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        self.pp.parse(s, err)?.value().into_parser().parse(s, err)
    }
}

/**
*/
pub struct Select<'a, S, E, A> {
    ps: Vec<Box<dyn Parser<S, E, Target = A> + 'a>>,
}

impl<'a, S, E, A> Select<'a, S, E, A> {
    pub fn new(ps: Vec<Box<dyn Parser<S, E, Target = A> + 'a>>) -> Self {
        Self { ps }
    }
}

impl<'a, S, E, A> Parser<S, E> for Select<'a, S, E, A>
where
    S: Clone,
    E: Clone,
{
    type Target = A;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        let mut iter = self.ps.iter();
        while let Some(p) = iter.next() {
            let s0 = s.clone();
            let err0 = err.clone();
            match p.parse(s, err) {
                Consumed::Empty(Some(x)) => {
                    return if let Some(q) = iter.next() {
                        let s1 = s.clone();
                        let err1 = err.clone();
                        *s = s0;
                        *err = err0;
                        match q.parse(s, err) {
                            Consumed::Empty(_) => {
                                *s = s1;
                                *err = err1;
                                Consumed::Empty(Some(x))
                            }
                            consumed => consumed,
                        }
                    } else {
                        Consumed::Empty(Some(x))
                    }
                }
                Consumed::Empty(None) => {
                    *s = s0;
                    *err = err0;
                }
                consumed => return consumed,
            }
        }
        Consumed::Empty(None)
    }
}

/**
*/
pub struct Join<'a, S, E, A> {
    ps: Vec<Box<dyn Parser<S, E, Target = A> + 'a>>,
}

impl<'a, S, E, A> Join<'a, S, E, A> {
    pub fn new(ps: Vec<Box<dyn Parser<S, E, Target = A> + 'a>>) -> Self {
        Self { ps }
    }
}

impl<'a, S, E, A> Parser<S, E> for Join<'a, S, E, A> {
    type Target = Vec<A>;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        let mut result = vec![];
        for p in self.ps.iter().take(self.ps.len() - 1) {
            result.push(p.parse(s, err)?.value());
        }
        match self.ps.last() {
            Some(p) => match p.parse(s, err)? {
                Consumed::Some(x) => {
                    result.push(x);
                    Consumed::Some(Some(result))
                }
                Consumed::Empty(x) => {
                    result.push(x);
                    Consumed::Empty(Some(result))
                }
            },
            None => Consumed::Empty(Some(result)),
        }
    }
}

/**
*/
#[derive(Copy, Clone, Debug)]
pub struct Many<P> {
    parser: P,
}

impl<P> Many<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S, E, P> Parser<S, E> for Many<P>
where
    S: Clone,
    E: Clone,
    P: Parser<S, E>,
{
    type Target = Vec<P::Target>;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        let mut result = vec![];
        let mut s0 = s.clone();
        let mut err0 = err.clone();
        let mut cons: fn(_) -> _ = Consumed::Empty;
        return loop {
            match self.parser.parse(s, err) {
                Consumed::Some(Some(x)) => {
                    result.push(x);
                    s0 = s.clone();
                    err0 = err.clone();
                    cons = Consumed::Some;
                }
                Consumed::Empty(Some(x)) => {
                    result.push(x);
                    s0 = s.clone();
                    err0 = err.clone();
                    cons = Consumed::Empty;
                }
                _ => {
                    *s = s0;
                    *err = err0;
                    break cons(Some(result));
                }
            }
        };
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Many_<P> {
    parser: P,
}

impl<P> Many_<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S, E, P> Parser<S, E> for Many_<P>
where
    S: Clone,
    E: Clone,
    P: Parser<S, E>,
{
    type Target = ();

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        let mut s0 = s.clone();
        let mut err0 = err.clone();
        let mut cons: fn(_) -> _ = Consumed::Empty;
        return loop {
            match self.parser.parse(s, err) {
                Consumed::Some(Some(x)) => {
                    s0 = s.clone();
                    err0 = err.clone();
                    cons = Consumed::Some;
                }
                Consumed::Empty(Some(x)) => {
                    s0 = s.clone();
                    err0 = err.clone();
                    cons = Consumed::Empty;
                }
                _ => {
                    *s = s0;
                    *err = err0;
                    break cons(Some(()));
                }
            }
        };
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Some<P> {
    parser: P,
}

impl<P> Some<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S, E, P> Parser<S, E> for Some<P>
where
    S: Clone,
    E: Clone,
    P: Parser<S, E>,
{
    type Target = Vec<P::Target>;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        let mut result = vec![];
        let mut cons: fn(_) -> _ = match self.parser.parse(s, err)? {
            Consumed::Some(x) => {
                result.push(x);
                Consumed::Some
            }
            Consumed::Empty(x) => {
                result.push(x);
                Consumed::Empty
            }
        };

        let mut s0 = s.clone();
        let mut err0 = err.clone();
        return loop {
            match self.parser.parse(s, err) {
                Consumed::Some(Some(x)) => {
                    result.push(x);
                    s0 = s.clone();
                    err0 = err.clone();
                    cons = Consumed::Some;
                }
                Consumed::Empty(Some(x)) => {
                    result.push(x);
                    s0 = s.clone();
                    err0 = err.clone();
                    cons = Consumed::Empty;
                }
                _ => {
                    *s = s0;
                    *err = err0;
                    break cons(Some(result));
                }
            }
        };
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Some_<P> {
    parser: P,
}

impl<P> Some_<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S, E, P> Parser<S, E> for Some_<P>
    where
        S: Clone,
        E: Clone,
        P: Parser<S, E>,
{
    type Target = ();

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        let mut cons: fn(_) -> _ = match self.parser.parse(s, err)? {
            Consumed::Some(_) => Consumed::Some,
            Consumed::Empty(x) => Consumed::Empty,

        };

        let mut s0 = s.clone();
        let mut err0 = err.clone();
        return loop {
            match self.parser.parse(s, err) {
                Consumed::Some(Some(x)) => {
                    s0 = s.clone();
                    err0 = err.clone();
                    cons = Consumed::Some;
                }
                Consumed::Empty(Some(x)) => {
                    s0 = s.clone();
                    err0 = err.clone();
                    cons = Consumed::Empty;
                }
                _ => {
                    *s = s0;
                    *err = err0;
                    break cons(Some(()));
                }
            }
        };
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Try<P> {
    parser: P,
}

impl<P> Try<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S, E, P> Parser<S, E> for Try<P>
where
    S: Clone,
    E: Clone,
    P: Parser<S, E>,
{
    type Target = Option<P::Target>;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        let s0 = s.clone();
        let err0 = err.clone();
        match self.parser.parse(s, err) {
            Consumed::Some(None) => {
                *s = s0;
                *err = err0;
                Consumed::Some(None)
            },
            Consumed::Empty(None) => {
                *s = s0;
                *err = err0;
                Consumed::Empty(None)
            },
            Consumed::Some(x) => Consumed::Some(x.map(Some)),
            Consumed::Empty(x) => Consumed::Empty(x.map(Some)),
        }
    }
}

/**
*/
#[derive(Debug, Clone)]
pub struct Info<P> {
    msg: String,
    parser: P,
}

impl<P> Info<P> {
    pub fn new(msg: &str, parser: P) -> Self {
        Self {
            msg: msg.to_string(),
            parser,
        }
    }
}

impl<S, E: ParseLogger, P: Parser<S, E>> Parser<S, E> for Info<P> {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        err.info(&self.msg);
        self.parser.parse(s, err)
    }
}

#[derive(Debug, Clone)]
pub struct Warn<P> {
    msg: String,
    parser: P,
}

impl<P> Warn<P> {
    pub fn new(msg: &str, parser: P) -> Self {
        Self {
            msg: msg.to_string(),
            parser,
        }
    }
}

impl<S, E: ParseLogger, P: Parser<S, E>> Parser<S, E> for Warn<P> {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        err.warn(&self.msg);
        self.parser.parse(s, err)
    }
}

#[derive(Debug, Clone)]
pub struct Err<P> {
    msg: String,
    parser: P,
}

impl<P> Err<P> {
    pub fn new(msg: &str, parser: P) -> Self {
        Self {
            msg: msg.to_string(),
            parser,
        }
    }
}

impl<S, E: ParseLogger, P: Parser<S, E>> Parser<S, E> for Err<P> {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        err.err(&self.msg);
        self.parser.parse(s, err)
    }
}
