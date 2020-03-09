use crate::core::traits::err::{ParseErr, ParserLogger};
use crate::{ParseState, Stream};
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{BitOr, Shl, Shr};

/// An interface for dealing with Parser Combinator.
pub trait Parser<S, E = ParseErr> {
    /// The type of the target after parsed.
    type Target;
    /**
    Parse the stream and returning the result with the rest of stream.
    Returns Err(ParseMsg) when the parsing failed or at the end of the stream.

    # Example
    Basic usage:
    ```
    use psc::core::traits::err::ParseErr;
    use psc::refactor::{char, Parser, ParserExt};
    use psc::ParseState;

    let parser = char('+').or(char('-')).option();
    // ('+'|'-')?

    let mut src = ParseState::new("+123");
    let mut logger = ParseErr::default();
    let res = parser.parse(&mut src, &mut logger).value().unwrap();
    assert_eq!(res, Some('+'));
    assert_eq!(src.as_str(), "123");

    let mut src = ParseState::new("-123");
    let mut logger = ParseErr::default();
    let res = parser.parse(&mut src, &mut logger).value().unwrap();
    assert_eq!(res, Some('-'));
    assert_eq!(src.as_str(), "123");

    let mut src = ParseState::new("123");
    let mut logger = ParseErr::default();
    let res = parser.parse(&mut src, &mut logger).value().unwrap();
    assert_eq!(res, None);
    assert_eq!(src.as_str(), "123");
    ```
    */
    fn parse(&self, s: &mut S, err: &mut E) -> ParseResult<Self::Target>;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Consumed<A> {
    Some(A),
    Empty(A),
}

pub type ParseResult<A> = Consumed<Option<A>>;

pub trait ParserExt<S, E>: Parser<S, E> {
    /**
    # Map Combinator.
    The parser `p.map(f)` creates an parser which calls the closure `f` on the parse result of p.

    Functor law satisfied:
    1. **identity**: `p.map(|x| x) ~ p`
    2. **composition**: `p.map(f).map(g) ~ p.map(|x| g(f(x))`
    ## Example
    ```
    use psc::core::traits::err::ParseErr;
    use psc::refactor::{digit, Parser, ParserExt};
    use psc::ParseState;

    let parser = digit().map(|c: char| c.to_digit(10)).map(Option::unwrap);

    let mut src = ParseState::new("1abc");
    let mut logger = ParseErr::default();
    let res = parser.parse(&mut src, &mut logger).value().unwrap();
    assert_eq!(res, 1);
    assert_eq!(src.as_str(), "abc");
    ```
    */
    fn map<B, F>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Target) -> B,
    {
        Map::new(self, f)
    }

    /**
    # Map2 Combinator
    It's just like `.map()`, but it map two parsers.
    ## Example
    ```
    use psc::core::traits::err::ParseErr;
    use psc::refactor::{char, digit, Parser, ParserExt};
    use psc::ParseState;

    let pa = char('1').map(|c: char| c.to_digit(10)).map(Option::unwrap);
    let pb = digit().map(|c: char| c.to_digit(10)).map(Option::unwrap);
    let parser = pa.map2(pb, |a, b| a + b);
    // 1[0-9]

    let mut src = ParseState::new("123");
    let mut logger = ParseErr::default();
    let res = parser.parse(&mut src, &mut logger).value().unwrap();
    assert_eq!(res, 3);
    ```
    */
    fn map2<U, B, F>(self, other: U, f: F) -> Map2<Self, U::Parser, F>
    where
        Self: Sized,
        U: IntoParser<S, E>,
        F: Fn(Self::Target, U::Target) -> B,
    {
        Map2::new(self, other.into_parser(), f)
    }

    /**
    # Context Sensitive Sequence Combinator.
    It's used to construct context sensitive parser.

    Monad law satisfied:
    1. **left identity**: pure(|| x).and_then(f) ~ f(x)
    2. **right identity**: p.and_then(|x| pure(|| x)) ~ p
    3. **associative**: p.and_then(f).and_then(g) ~ p.and_then(|x| f(x).and_then(g))

    ## Example
    ```
    use psc::core::traits::err::ParseErr;
    use psc::refactor::{char, digit, satisfy, Parser, ParserExt};
    use psc::ParseState;
    let parser = satisfy(|_| true).and_then(|upper: char| {
        if upper.is_uppercase() {
            char('1')
        } else {
            char('2')
        }
    });
    // [A-Z]1 | [a-z]2

    let res = parser.parse(&mut ParseState::new("H1"), &mut ParseErr::default()).value().unwrap();
    assert_eq!(res, '1');

    let res = parser.parse(&mut ParseState::new("h2"), &mut ParseErr::default()).value().unwrap();
    assert_eq!(res, '2');
    ```
    */
    fn and_then<U, F>(self, f: F) -> AndThen<Self, F>
    where
        Self: Sized,
        U: IntoParser<S, E>,
        F: Fn(Self::Target) -> U,
    {
        AndThen::new(self, f)
    }

    /**
    # Alternative combinator.
    The parser `p.or(q)` first applies `p`. If it succeeds, the value of `p` is returned.
    If `p` *fails without consuming any input*, parser `q` is tried.
    The parser is called *predictive* since `q` is only tried when parser `p` didn't consume any input.

    Monoid law satisfied:
    1. **Associative**: `p.or(q.or(r)) ~ p.or(q).or(r)`
    2. **identity**: `p.or(empty()) ~ p ~ empty().or(p)`

    ## Example
    ```
    use psc::core::traits::err::ParseErr;
    use psc::refactor::{char, IntoParser, Parser, ParserExt};
    use psc::ParseState;

    let parser = char('+').or(char('-').or(char('*'))).or(char('/'));
    // '+' | ('-' | '*') | '/'

    let mut src = ParseState::new("+123");
    let mut logger = ParseErr::default();
    let res = parser.parse(&mut src, &mut logger).value().unwrap();
    assert_eq!(res, '+');
    assert_eq!(src.as_str(), "123");

    let mut src = ParseState::new("-123");
    let mut logger = ParseErr::default();
    let res = parser.parse(&mut src, &mut logger).value().unwrap();
    assert_eq!(res, '-');
    assert_eq!(src.as_str(), "123");

    let mut src = ParseState::new("*123");
    let mut logger = ParseErr::default();
    let res = parser.parse(&mut src, &mut logger).value().unwrap();
    assert_eq!(res, '*');
    assert_eq!(src.as_str(), "123");

    let mut src = ParseState::new("/123");
    let res = parser.parse(&mut src, &mut logger).value().unwrap();

    assert_eq!(res, '/');
    assert_eq!(src.as_str(), "123");
    ```
    */
    fn or<U>(self, other: U) -> Or<Self, U::Parser>
    where
        Self: Sized,
        U: IntoParser<S, E, Target = Self::Target>,
    {
        Or::new(self, other.into_parser())
    }

    /**
    # Sequence Combinator
    The parser `p.and_r(q)` applies `p` and `q` sequent and returns the result of `q`.
    If `p` fails, `q` won't be called.

    Associative law satisfied:
    `p.and_r(q.and_r(r)) ~ p.and_r(q).and_r(r)`

    ## Example
    ```
    use psc::core::traits::err::ParseErr;
    use psc::refactor::{char, IntoParser, Parser, ParserExt};
    use psc::ParseState;
    let parser = char('a').and_r(char('b').and_r(char('c'))).and_r(char('d'));

    let mut src = ParseState::new("abcde");
    let mut logger = ParseErr::default();
    let res = parser.parse(&mut src, &mut logger).value().unwrap();
    assert_eq!(res, 'd');
    assert_eq!(src.as_str(), "e");
    ```
    */
    fn and_r<U>(self, other: U) -> AndR<Self, U::Parser>
    where
        Self: Sized,
        U: IntoParser<S, E>,
    {
        AndR::new(self, other.into_parser())
    }

    /**
    # Sequence Combinator
    The parser `p.and_r(q)` applies `p` and `q` sequent and returns the result of `p`.
    If `p` fails, `q` won't be called.

    Associative law satisfied:
    `p.and_l(q.and_l(r)) ~ p.and_l(q).and_l(r)`

    ## Example
    ```
    use psc::core::traits::err::ParseErr;
    use psc::refactor::{char, IntoParser, Parser, ParserExt};
    use psc::ParseState;
    let parser = char('a').and_l(char('b').and_l(char('c'))).and_l(char('d'));

    let mut src = ParseState::new("abcde");
    let mut logger = ParseErr::default();
    let res = parser.parse(&mut src, &mut logger).value().unwrap();
    assert_eq!(res, 'a');
    assert_eq!(src.as_str(), "e");
    ```
    */
    fn and_l<U>(self, other: U) -> AndL<Self, U::Parser>
    where
        Self: Sized,
        U: IntoParser<S, E>,
    {
        AndL::new(self, other.into_parser())
    }

    /// It can be defined as `pp.flatten() ~ pp.and_then(|x| x)`
    fn flatten(self) -> Flatten<Self>
    where
        Self: Sized,
        Self::Target: IntoParser<S, E>,
    {
        Flatten::new(self)
    }

    /**
    # Attempt Combinator
    The parser p.attempt() behaves like parser p,
    except that it pretends that it hasn't consumed any input when an error occurs.
    ## Example
    ```
    use psc::core::traits::err::ParseErr;
    use psc::refactor::{char, strg, IntoParser, Parser, ParserExt};
    use psc::ParseState;

    let parser = char('p').and_r("pq").attempt().or("pq");

    let mut src = ParseState::new("pq");
    let mut logger = ParseErr::default();
    let res = parser.parse(&mut src, &mut logger).value();
    assert_eq!(res, Some("pq"));
    assert_eq!(src.as_str(), "");

    let parser = char('p').and_r("pq").or("pq");

    let mut src = ParseState::new("pq");
    let mut logger = ParseErr::default();
    let res = parser.parse(&mut src, &mut logger).value();
    assert_eq!(res, None);
    ```
    */
    fn attempt(self) -> Attempt<Self>
    where
        Self: Sized,
    {
        Attempt::new(self)
    }

    /**
    # Kleene Closure Combinator
    `p.many()` applies the parser `p` zero or more times.
    Returns a vec of the returned values of `p`.

    ## Example
    ```
    use psc::core::traits::err::ParseErr;
    use psc::refactor::{char, strg, IntoParser, Parser, ParserExt};
    use psc::ParseState;

    let parser = char('0').or('1').many();
    let mut src = ParseState::new("0110");
    let mut logger = ParseErr::default();
    let res = parser.parse(&mut src, &mut logger).value().unwrap();
    assert_eq!(res, vec!['0', '1', '1', '0']);
    assert_eq!(src.as_str(), "");
    ```
    */
    fn many(self) -> Many<Self>
    where
        Self: Sized,
    {
        Many::new(self)
    }

    /**
    Kleene Closure Combinator
    `p.many_()` applies the parser `p` zero or more times, ignore its result.
    */
    fn many_(self) -> Many_<Self>
    where
        Self: Sized,
    {
        Many_::new(self)
    }

    /**
    `p.some()` applies the parser `p` one or more times.
    Returns a vec of the returned values of `p`.
    */
    fn some(self) -> Some<Self>
    where
        Self: Sized,
    {
        Some::new(self)
    }

    /// `p.some_()` applies the parser `p` one or more times, ignore its result.
    fn some_(self) -> Some_<Self>
    where
        Self: Sized,
    {
        Some_::new(self)
    }

    /// `p.option()` applies the parser `p` zero or one times.
    fn option(self) -> Optional<Self>
    where
        Self: Sized,
    {
        Optional::new(self)
    }

    /// append some info when its fail
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

    pub fn consumed(self) -> Self {
        Consumed::Some(self.value())
    }

    pub fn cancel(self) -> Self {
        Consumed::Empty(self.value())
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
            Consumed::Some(Some(x)) => f(x).consumed(),
            Consumed::Empty(Some(x)) => f(x),
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
        self.pa.parse(s, err).flat_map(|_| self.pb.parse(s, err))

    }
}


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
        self.pa.parse(s, err).flat_map(|a|self.pb.parse(s, err).map(move |_| a))
    }
}


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
        self.pp.parse(s, err).flat_map(|p| p.into_parser().parse(s, err))
    }
}


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
        let mut cons: fn(_) -> _ = Consumed::Empty;
        for p in self.ps.iter() {
            match p.parse(s, err)? {
                Consumed::Some(x) => {
                    result.push(x);
                    cons = Consumed::Some;
                }
                Consumed::Empty(x) => {
                    result.push(x);
                }
            }
        }
        cons(Some(result))
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
        loop {
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
                }
                _ => {
                    *s = s0;
                    *err = err0;
                    break cons(Some(result));
                }
            }
        }
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
        loop {
            match self.parser.parse(s, err) {
                Consumed::Some(Some(_)) => {
                    s0 = s.clone();
                    err0 = err.clone();
                    cons = Consumed::Some;
                }
                Consumed::Empty(Some(_)) => {
                    s0 = s.clone();
                    err0 = err.clone();
                }
                _ => {
                    *s = s0;
                    *err = err0;
                    break cons(Some(()));
                }
            }
        }
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
        loop {
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
                }
                _ => {
                    *s = s0;
                    *err = err0;
                    break cons(Some(result));
                }
            }
        }
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
            Consumed::Empty(_) => Consumed::Empty,
        };

        let mut s0 = s.clone();
        let mut err0 = err.clone();
        loop {
            match self.parser.parse(s, err) {
                Consumed::Some(Some(_)) => {
                    s0 = s.clone();
                    err0 = err.clone();
                    cons = Consumed::Some;
                }
                Consumed::Empty(Some(_)) => {
                    s0 = s.clone();
                    err0 = err.clone();
                }
                _ => {
                    *s = s0;
                    *err = err0;
                    break cons(Some(()));
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Optional<P> {
    parser: P,
}

impl<P> Optional<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S, E, P> Parser<S, E> for Optional<P>
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
                Consumed::Some(Some(None))
            }
            Consumed::Empty(None) => {
                *s = s0;
                *err = err0;
                Consumed::Empty(Some(None))
            }
            Consumed::Some(x) => Consumed::Some(x.map(Some)),
            Consumed::Empty(x) => Consumed::Empty(x.map(Some)),
        }
    }
}


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

/**
# Parser Operator
## Example
```
use psc::refactor::wrap;

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
    type Output =  ParserWrapper<S, E, Or<P::Parser, Q::Parser>>;

    fn bitor(self, rhs: Q) -> Self::Output {
        ParserWrapper::new(Or::new(self.inner.into_parser(), rhs.into_parser()))
    }
}

impl<S, E, P, Q> Shl<Q> for ParserWrapper<S, E, P>
where
    P: IntoParser<S, E>,
    Q: IntoParser<S, E>,
{
    type Output =  ParserWrapper<S, E, AndL<P::Parser, Q::Parser>>;

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
            _marker: PhantomData,
        }
    }
}

impl<E, F: Copy> Copy for Satisfy<E, F> {}

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
    satisfy(|c| c.is_digit(10)).info("expecting digit")
}

#[derive(Debug)]
pub struct Char<E> {
    ch: char,
    _marker: PhantomData<fn(&mut E)>,
}

impl<E> Char<E> {
    pub fn new(ch: char) -> Self {
        Self {
            ch,
            _marker: PhantomData,
        }
    }
}

impl<E> Clone for Char<E> {
    fn clone(&self) -> Self {
        Self {
            ch: self.ch,
            _marker: PhantomData,
        }
    }
}

impl<E> Copy for Char<E> {}

impl<'a, E: ParserLogger> Parser<ParseState<'a>, E> for Char<E> {
    type Target = char;

    fn parse(&self, s: &mut ParseState<'a>, err: &mut E) -> Consumed<Option<Self::Target>> {
        match s.next() {
            Some(ch) => {
                if self.ch == ch {
                    Consumed::Some(Some(ch))
                } else {
                    err.clear();
                    err.err(&format!(
                        "err at {:?}, unexpect '{}' expecting '{}'",
                        s.pos, ch, self.ch
                    ));
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

pub fn char<E: ParserLogger>(ch: char) -> Char<E> {
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
            _marker: PhantomData,
        }
    }
}

impl<E> Clone for Strg<E> {
    fn clone(&self) -> Self {
        Self {
            temp: self.temp.to_owned(),
            _marker: PhantomData,
        }
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
            err.err(&format!(
                "error at {:?}, expecting \"{}\"",
                s.pos, self.temp
            ));
            Consumed::Empty(None)
        }
    }
}

pub fn strg<E: ParserLogger>(temp: &str) -> Strg<E> {
    Strg::new(temp)
}

pub struct EOF<E> {
    _marker: PhantomData<fn(&mut E)>,
}


impl<E> EOF<E> {
    pub fn new() -> Self {
        Self { _marker: PhantomData }
    }
}

impl<E> Clone for EOF<E> {
    fn clone(&self) -> Self {
        Self::new()
    }
}

impl<E> Copy for EOF<E> { }

impl<E> Default for EOF<E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, E: ParserLogger> Parser<ParseState<'a>, E> for EOF<E> {
    type Target = ();

    fn parse(&self, s: &mut ParseState<'a>, err: &mut E) -> Consumed<Option<Self::Target>> {
        match s.next() {
            None => Consumed::Empty(Some(())),
            Some(x) => {
                err.clear();
                err.err(&format!("error at {:?}, expecting eof, unexpected '{}'", s.pos, x));
                Consumed::Empty(None)
            },
        }
    }
}


impl<'a> IntoParser<ParseState<'a>, ParseErr> for char {
    type Target = char;
    type Parser = Char<ParseErr>;

    fn into_parser(self) -> Self::Parser {
        Char::new(self)
    }
}

impl<'a> IntoParser<ParseState<'a>, ParseErr> for &str {
    type Target = &'a str;
    type Parser = Strg<ParseErr>;

    fn into_parser(self) -> Self::Parser {
        Strg::new(self)
    }
}

mod tests;
