use crate::core::traits::err::{ParseErr, ParserLogger};
use crate::{ParseState, Stream};
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{BitOr, Shl, Shr};

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

pub trait ParserExt<S, E>: Parser<S, E> {
    fn map<B, F>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Target) -> B,
    {
        Map::new(self, f)
    }

    fn map2<U, B, F>(self, other: U, f: F) -> Map2<Self, U::Parser, F>
    where
        Self: Sized,
        U: IntoParser<S, E>,
        F: Fn(Self::Target, U::Target) -> B,
    {
        Map2::new(self, other.into_parser(), f)
    }

    fn and_then<U, F>(self, f: F) -> AndThen<Self, F>
    where
        Self: Sized,
        U: IntoParser<S, E>,
        F: Fn(Self::Target) -> U,
    {
        AndThen::new(self, f)
    }

    fn flatten(self) -> Flatten<Self>
    where
        Self: Sized,
        Self::Target: IntoParser<S, E>,
    {
        Flatten::new(self)
    }

    fn attempt(self) -> Attempt<Self>
    where
        Self: Sized,
    {
        Attempt::new(self)
    }

    fn many(self) -> Many<Self>
    where
        Self: Sized,
    {
        Many::new(self)
    }

    fn many_(self) -> Many_<Self>
    where
        Self: Sized,
    {
        Many_::new(self)
    }

    fn some(self) -> Some<Self>
    where
        Self: Sized,
    {
        Some::new(self)
    }

    fn some_(self) -> Some_<Self>
    where
        Self: Sized,
    {
        Some_::new(self)
    }

    fn r#try(self) -> Try<Self>
    where
        Self: Sized,
    {
        Try::new(self)
    }

    fn info(self, msg: &str) -> Info<Self>
    where
        Self: Sized,
    {
        Info::new(msg, self)
    }

    fn warn(self, msg: &str) -> Warn<Self>
    where
        Self: Sized,
    {
        Warn::new(msg, self)
    }

    fn err(self, msg: &str) -> Err<Self>
    where
        Self: Sized,
    {
        Err::new(msg, self)
    }

    fn wrap(self) -> ParserWrapper<S, E, Self>
    where
        Self: Sized,
    {
        ParserWrapper::new(self)
    }
}

impl<S, E, P: Parser<S, E>> ParserExt<S, E> for P {}

impl<S, E, P: Parser<S, E> + ?Sized> Parser<S, E> for &P {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        (**self).parse(s, err)
    }
}

impl<S, E, P: Parser<S, E> + ?Sized> Parser<S, E> for &mut P {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        (**self).parse(s, err)
    }
}

impl<S, E, P: Parser<S, E> + ?Sized> Parser<S, E> for Box<P> {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        (**self).parse(s, err)
    }
}

impl<S, E, P: Parser<S, E> + ?Sized> Parser<S, E> for std::rc::Rc<P> {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        (**self).parse(s, err)
    }
}

impl<S, E, P: Parser<S, E> + ?Sized> Parser<S, E> for std::sync::Arc<P> {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        (**self).parse(s, err)
    }
}

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
    _marker: PhantomData<fn(&mut S, &mut E) -> Consumed<Option<A>>>,
}

impl<A, S, E> Empty<S, E, A> {
    pub fn new() -> Self {
        Self {
            _marker: PhantomData,
        }
    }
}

pub fn empty<S, E, A>() -> Empty<S, E, A> {
    Empty::new()
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
    _marker: PhantomData<fn(&mut S, &mut E)>,
}

impl<S, E, F> Pure<S, E, F> {
    pub fn new(x: F) -> Self {
        Self {
            inner: x,
            _marker: PhantomData,
        }
    }
}

pub fn pure<S, E, A, F>(x: F) -> Pure<S, E, F>
where
    F: Fn() -> A,
{
    Pure::new(x)
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

pub fn fail<S, E, A>(msg: &str) -> Fail<S, E, A> {
    Fail::new(msg)
}

impl<S, E: ParserLogger, A> Parser<S, E> for Fail<S, E, A> {
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

pub fn select<'a, S, E, A>(ps: Vec<Box<dyn Parser<S, E, Target = A> + 'a>>) -> Select<'a, S, E, A> {
    Select::new(ps)
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

pub fn join<'a, S, E, A>(ps: Vec<Box<dyn Parser<S, E, Target = A> + 'a>>) -> Join<'a, S, E, A> {
    Join::new(ps)
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
            }
            Consumed::Empty(None) => {
                *s = s0;
                *err = err0;
                Consumed::Empty(None)
            }
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

impl<S, E: ParserLogger, P: Parser<S, E>> Parser<S, E> for Info<P> {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        match self.parser.parse(s, err) {
            Consumed::Empty(None) => {
                err.info(&self.msg);
                Consumed::Empty(None)
            }
            Consumed::Some(None) => {
                err.info(&self.msg);
                Consumed::Empty(None)
            }
            ok => ok,
        }
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

impl<S, E: ParserLogger, P: Parser<S, E>> Parser<S, E> for Warn<P> {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        match self.parser.parse(s, err) {
            Consumed::Empty(None) => {
                err.warn(&self.msg);
                Consumed::Empty(None)
            }
            Consumed::Some(None) => {
                err.warn(&self.msg);
                Consumed::Empty(None)
            }
            ok => ok,
        }
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

impl<S, E: ParserLogger, P: Parser<S, E>> Parser<S, E> for Err<P> {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        match self.parser.parse(s, err) {
            Consumed::Empty(None) => {
                err.err(&self.msg);
                Consumed::Empty(None)
            }
            Consumed::Some(None) => {
                err.err(&self.msg);
                Consumed::Empty(None)
            }
            ok => ok,
        }
    }
}

/**
ops
*/
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

impl<S, E, P: Parser<S, E>> Parser<S, E> for ParserWrapper<S, E, P> {
    type Target = P::Target;

    fn parse(&self, s: &mut S, err: &mut E) -> Consumed<Option<Self::Target>> {
        self.inner.parse(s, err)
    }
}

impl<S, E, P, Q> BitOr<Q> for ParserWrapper<S, E, P>
where
    P: Parser<S, E>,
    Q: IntoParser<S, E, Target = P::Target>,
{
    type Output = Or<P, Q>;

    fn bitor(self, rhs: Q) -> Self::Output {
        Or::new(self.inner, rhs)
    }
}

impl<S, E, P, Q> Shl<Q> for ParserWrapper<S, E, P>
where
    P: Parser<S, E>,
    Q: IntoParser<S, E>,
{
    type Output = AndL<P, Q>;

    fn shl(self, rhs: Q) -> Self::Output {
        AndL::new(self.inner, rhs)
    }
}

impl<S, E, P, Q> Shr<Q> for ParserWrapper<S, E, P>
where
    P: Parser<S, E>,
    Q: IntoParser<S, E>,
{
    type Output = AndR<P, Q>;

    fn shr(self, rhs: Q) -> Self::Output {
        AndR::new(self.inner, rhs)
    }
}

/**
*/
#[derive(Debug)]
pub struct Satisfy<E, F> {
    satisfy: F,
    _marker: PhantomData<fn(&mut E)>,
}

impl<E, F> Satisfy<E, F> {
    pub fn new(satisfy: F) -> Self {
        Self {
            satisfy,
            _marker: PhantomData,
        }
    }
}

impl<E, F: Clone> Clone for Satisfy<E, F> {
    fn clone(&self) -> Self {
        Self {
            satisfy: self.satisfy.clone(),
            _marker: PhantomData
        }
    }
}

impl<E, F: Copy> Copy for Satisfy<E, F> { }

impl<'a, E, F> Parser<ParseState<'a>, E> for Satisfy<E, F>
where
    E: ParserLogger,
    F: Fn(&char) -> bool,
{
    type Target = <ParseState<'a> as Iterator>::Item;

    fn parse(&self, s: &mut ParseState<'a>, err: &mut E) -> Consumed<Option<Self::Target>> {
        match s.next() {
            Some(ch) => {
                if (self.satisfy)(&ch) {
                    Consumed::Some(Some(ch))
                } else {
                    err.clear();
                    err.err(&format!("err at {:?}", s.pos));
                    Consumed::Empty(None)
                }
            }
            None => {
                err.clear();
                err.err(&format!("err at {:?} input exhausted", s.pos));
                Consumed::Empty(None)
            }
        }
    }
}

pub fn satisfy<E, F>(satisfy: F) -> Satisfy<E, F>
where
    E: ParserLogger,
    F: Fn(&char) -> bool,
{
    Satisfy::new(satisfy)
}

pub fn letter<E: ParserLogger>() -> Info<Satisfy<E, impl Fn(&char) -> bool + Copy>> {
    satisfy(|c| c.is_alphabetic()).info("expecting alphabetic")
}

pub fn digit<E: ParserLogger>() -> Info<Satisfy<E, impl Fn(&char) -> bool + Copy>> {
    satisfy(|c| c.is_digit(10)).info("expecting alphabetic")
}

#[derive(Debug)]
pub struct Char<E> {
    ch: char,
    _marker: PhantomData<fn(&mut E)>
}

impl<E> Char<E> {
    pub fn new(ch: char) -> Self {
        Self { ch, _marker: PhantomData }
    }
}

impl<E> Clone for Char<E> {
    fn clone(&self) -> Self {
        Self { ch: self.ch, _marker: PhantomData }
    }
}

impl<E> Copy for Char<E> { }

impl<'a, E: ParserLogger> Parser<ParseState<'a>, E> for Char<E> {
    type Target = char;

    fn parse(&self, s: &mut ParseState<'a>, err: &mut E) -> Consumed<Option<Self::Target>> {
        match s.next() {
            Some(ch) => {
                if self.ch == ch {
                    Consumed::Some(Some(ch))
                } else {
                    err.clear();
                    err.err(&format!("err at {:?}, unexpect {} expecting {}", s.pos, ch, self.ch));
                    Consumed::Empty(None)
                }
            }
            None => {
                err.clear();
                err.err(&format!("err at {:?} input exhausted", s.pos));
                Consumed::Empty(None)
            }
        }
    }
}

fn char<E: ParserLogger>(ch: char) -> Char<E> {
    Char::new(ch)
}

#[derive(Debug)]
pub struct Strg<E> {
    temp: String,
    _marker: PhantomData<fn(&mut E) -> &str>,
}

impl<E> Strg<E> {
    pub fn new(temp: &str) -> Self {
        Self {
            temp: temp.to_string(),
            _marker: PhantomData
        }
    }
}

impl<E> Clone for Strg<E> {
    fn clone(&self) -> Self {
        Self { temp: self.temp.to_owned(), _marker: PhantomData }
    }
}

impl<'a, E: ParserLogger> Parser<ParseState<'a>, E> for Strg<E> {
    type Target = &'a str;

    fn parse(&self, s: &mut ParseState<'a>, err: &mut E) -> Consumed<Option<Self::Target>> {
        let src = s.as_str();
        if let Some(0) = src.find(&self.temp) {
            s.take(self.temp.len()).for_each(|_| {});
            Consumed::Some(Some(src.split_at(self.temp.len()).0))
        } else {
            err.clear();
            err.err(&format!("error at {:?}, expecting \"{}\"", s.pos, self.temp));
            Consumed::Empty(None)
        }
    }
}

pub fn strg<E: ParserLogger>(temp: &str) -> Strg<E> {
    Strg::new(temp)
}