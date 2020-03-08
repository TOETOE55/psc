use crate::core::combinators::{
    AndL, AndR, AndThen, App, Chain, Cons, Failure, Join, Many, Many_, Map, Map2, Or, Snoc, Some,
    Some_, Try,
};
use crate::core::err::ParseMsg;
use crate::core::traits::covert::IntoParser;
use crate::core::traits::stream::Stream;
use crate::ParserWrapper;
use std::rc::Rc;

/// An interface for dealing with Parser Combinator.
/// This is the main parser trait.
pub trait Parser<S: Stream> {
    /// The type of the target after parsed.
    type Target;

    /// Parse the stream and returning the result with the rest of stream.
    /// Returns Err(ParseMsg) when the parsing failed or at the end of the stream.
    ///
    /// # Example
    /// Basic usage:
    /// ```
    /// use psc::{satisfy, char, Parser};
    ///
    /// let parser = char('+').or(char('-')).tries();
    /// // ('+'|'-')?
    ///
    /// let mut src = "+123".chars();
    /// let res = parser.parse(&mut src).unwrap();
    /// assert_eq!(res, Some('+'));
    /// assert_eq!(src.as_str(), "123");
    ///
    /// let mut src = "-123".chars();
    /// let res = parser.parse(&mut src).unwrap();
    /// assert_eq!(res, Some('-'));
    /// assert_eq!(src.as_str(), "123");
    ///
    /// let mut src = "123".chars();
    /// let res = parser.parse(&mut src).unwrap();
    /// assert_eq!(res, None);
    /// assert_eq!(src.as_str(), "123");
    /// ```
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg>;
}

impl<S: Stream, P: Parser<S> + ?Sized> Parser<S> for &P {
    type Target = P::Target;
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        (**self).parse(stream)
    }
}

impl<S: Stream, P: Parser<S> + ?Sized> Parser<S> for &mut P {
    type Target = P::Target;
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        (**self).parse(stream)
    }
}

impl<S: Stream, P: Parser<S> + ?Sized> Parser<S> for Box<P> {
    type Target = P::Target;
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        (**self).parse(stream)
    }
}

impl<S: Stream, P: Parser<S> + ?Sized> Parser<S> for Rc<P> {
    type Target = P::Target;
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        (**self).parse(stream)
    }
}

pub trait ParserExt<S: Stream>: Parser<S> {
    /// Alternative combinator.
    /// The parser `p.or(q)` first applies `p`. If it succeeds, the value of `p` is returned.
    /// If `p` *fails without consuming any input*, parser `q` is tried.
    /// The parser is called *predictive* since `q` is only tried when parser `p` didn't consume any input.
    ///
    /// Monoid law satisfied:
    /// 1. **Associative**: `p.or(q.or(r)) ~ p.or(q).or(r)`
    /// 2. **identity**: `p.or(failure(e)) ~ p ~ failure(e).or(p)`
    /// # Example
    /// ```
    /// use psc::{char, Parser};
    ///
    /// let parser = char('+').or(char('-').or(char('*'))).or(char('/'));
    /// // '+' | ('-' | '*') | '/'
    ///
    /// let mut src = "+123".chars();
    /// let res = parser.parse(&mut src).unwrap();
    /// assert_eq!(res, '+');
    /// assert_eq!(src.as_str(), "123");
    ///
    /// let mut src = "-123".chars();
    /// let res = parser.parse(&mut src).unwrap();
    /// assert_eq!(res, '-');
    /// assert_eq!(src.as_str(), "123");
    ///
    /// let mut src = "*123".chars();
    /// let res = parser.parse(&mut src).unwrap();
    /// assert_eq!(res, '*');
    /// assert_eq!(src.as_str(), "123");
    ///
    /// let mut src = "/123".chars();
    /// let res = parser.parse(&mut src).unwrap();
    /// assert_eq!(res, '/');
    /// assert_eq!(src.as_str(), "123");
    /// ```
    fn or<U>(self, other: U) -> Or<Self, U::Parser>
    where
        Self: Sized,
        U: IntoParser<S, Target = Self::Target>,
    {
        Or::new(self, other.into_parser())
    }

    /// Sequence Combinator.
    /// The parser `p.and_r(q)` applies `p` and `q` sequent and returns the result of `q`.
    /// If `p` fails, `q` won't be called.
    ///
    /// Associative law satisfied:
    /// `p.and_r(q.and_r(r)) ~ p.and_r(q).and_r(r)`
    ///
    /// # Example
    /// ```
    /// use psc::{char, Parser};
    ///
    /// let parser = char('a').and_r(char('b').and_r(char('c'))).and_r(char('d'));
    /// // a(bc)d
    ///
    /// let mut src = "abcde".chars();
    /// let res = parser.parse(&mut src).unwrap();
    /// assert_eq!(res, 'd');
    /// assert_eq!(src.as_str(), "e");
    ///
    /// let res = parser.parse(&mut "ab".chars()).ok();
    /// assert_eq!(res, None);
    ///
    /// let res = parser.parse(&mut "acde".chars()).ok();
    /// assert_eq!(res, None);
    /// ```
    fn and_r<U>(self, other: U) -> AndR<Self, U::Parser>
    where
        Self: Sized,
        U: IntoParser<S>,
    {
        AndR::new(self, other.into_parser())
    }

    /// Sequence Combinator.
    /// The parser `p.and_r(q)` applies `p` and `q` sequent and returns the result of `p`.
    /// If `p` fails, `q` won't be called.
    ///
    /// Associative law satisfied:
    /// `p.and_l(q.and_l(r)) ~ p.and_l(q).and_l(r)`
    ///
    /// # Example
    /// ```
    /// use psc::{char, Parser};
    ///
    /// let parser = char('a').and_r(char('b').and_l(char('c'))).and_l(char('d'));
    /// // a(bc)d
    ///
    /// let mut src = "abcde".chars();
    /// let res = parser.parse(&mut src).unwrap();
    /// assert_eq!(res, 'b');
    /// assert_eq!(src.as_str(), "e");
    ///
    /// let res = parser.parse(&mut "ab".chars()).ok();
    /// assert_eq!(res, None);
    ///
    /// let res = parser.parse(&mut "acde".chars()).ok();
    /// assert_eq!(res, None);
    /// ```
    fn and_l<U>(self, other: U) -> AndL<Self, U::Parser>
    where
        Self: Sized,
        U: IntoParser<S>,
    {
        AndL::new(self, other.into_parser())
    }

    /// Map Combinator.
    /// The parser `p.map(f)` creates an parser which calls the closure `f` on the parse result of p.
    ///
    /// Functor law satisfied:
    /// 1. **identity**: `p.map(|x| x) ~ p`
    /// 2. **composition**: `p.map(f).map(g) ~ p.map(|x| g(f(x))`
    /// ```
    /// use psc::{satisfy, Parser};
    /// let parser = satisfy(|ch: &char| ch.is_numeric())
    ///     .map(|c: char| c.to_digit(10))
    ///     .map(Option::unwrap);
    ///
    /// let mut src = "1abc".chars();
    /// let res = parser.parse(&mut src).unwrap();
    /// assert_eq!(res, 1);
    /// assert_eq!(src.as_str(), "abc");
    /// ```
    fn map<B, F>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Target) -> B,
    {
        Map::new(self, f)
    }

    /// Map2 Combinator
    /// It's just like `.map()`, but it map two parsers.
    ///
    /// # Example
    /// ```
    /// use psc::{char, satisfy, Parser};
    ///
    /// let pa = char('1').map(|c: char| c.to_digit(10)).map(Option::unwrap);
    /// let pb = satisfy(|ch: &char| ch.is_numeric()).map(|c: char| c.to_digit(10)).map(Option::unwrap);
    /// let parser = pa.map2(pb, |a, b| a+b);
    /// // 1[0-9]
    ///
    /// let res = parser.parse(&mut "123".chars()).unwrap();
    /// assert_eq!(res, 3);
    /// ```
    fn map2<U, B, F>(self, other: U, f: F) -> Map2<Self, U::Parser, F>
    where
        Self: Sized,
        U: IntoParser<S>,
        F: Fn(Self::Target, U::Target) -> B,
    {
        Map2::new(self, other.into_parser(), f)
    }

    /// Applicative Combinator.
    /// The parser `u.app(v)` applies `u` and `v` sequent
    /// and take the result of `u` to apply to the result of `v`.
    /// It can be defined as `u.app(v) ~ Parser::map2(u, v, |f, x| f(x))`.
    /// `mapN` can be easily defined by `app`.
    /// (i.e. `map3(u, v, w, |x, y, z| ...) ~ pure(|| |x| |y| |z| ...).app(u).app(v).app(w)`)
    ///
    /// Applicative Functor law satisfied:
    /// 1. **identity**: `pure(|| |x| x).app(v) ~ v`
    /// 2. **composition**: `pure(|| |f| |g| |x| f(g(x))).app(u).app(v).app(w) ~ u.app(v.app(w))`
    /// 3. **homomorphism**: `pure(|| x).app(pure(|| f)) ~ pure(|| f(x))`
    /// 4. **interchange**: `pure(|| x).app(u) ~ u.app(pure(|| |x| |f| f(x)))`
    ///
    /// As a consequence of these laws, it will satisfy:
    /// 1. `pure(|| f).app(p) ~ p.map(f)`
    fn app<U, T, F>(self, other: U) -> App<Self, U::Parser>
    where
        Self: Parser<S, Target = F> + Sized,
        U: IntoParser<S>,
        F: Fn(U::Target) -> T,
    {
        App::new(self, other.into_parser())
    }

    /// Context Sensitive Sequence Combinator.
    /// It's used to construct context sensitive parser.
    ///
    /// Monad law satisfied:
    /// 1. **left identity**: pure(|| x).and_then(f) ~ f(x)
    /// 2. **right identity**: p.and_then(|x| pure(|| x)) ~ p
    /// 3. **associative**: p.and_then(f).and_then(g) ~ p.and_then(|x| f(x).and_then(g))
    ///
    /// # Example
    /// ```
    /// use psc::{satisfy, Parser, char};
    ///
    /// let parser = satisfy(|_| true)
    ///     .and_then(|upper: char| if upper.is_uppercase() {
    ///         char('1')
    ///     } else {
    ///         char('2')
    ///     });
    /// // [A-Z]1 | [a-z]2
    ///
    /// let res = parser.parse(&mut "H1".chars()).unwrap();
    /// assert_eq!(res, '1');
    ///
    /// let res = parser.parse(&mut "h2".chars()).unwrap();
    /// assert_eq!(res, '2');
    /// ```
    fn and_then<U, F>(self, f: F) -> AndThen<Self, F>
    where
        Self: Sized,
        U: IntoParser<S>,
        F: Fn(Self::Target) -> U,
    {
        AndThen::new(self, f)
    }

    /// Parse in sequence, and collect the result.
    /// # Example
    /// ```
    /// use psc::{char, Parser, ParseState};
    ///
    /// let parser = char('2').cons(char('3').cons(char('3').many()));
    /// // 23(3*)
    ///
    /// let mut src = ParseState::new("23334");
    /// let res = parser.parse(&mut src).unwrap();
    /// assert_eq!(res, vec!['2', '3', '3', '3']);
    /// assert_eq!(src.as_str(), "4");
    /// ```
    fn cons<U>(self, other: U) -> Cons<Self, U::Parser>
    where
        Self: Sized,
        U: IntoParser<S, Target = Vec<Self::Target>>,
    {
        Cons::new(self, other.into_parser())
    }

    /// Parse in sequence, and collect the result.
    /// # Example
    /// ```
    /// use psc::{char, Parser, ParseState};
    ///
    /// let parser = char('3').many().snoc(char('2'));
    /// // (3*)2
    ///
    /// let res = parser.parse(&mut "3332".chars()).unwrap();
    /// assert_eq!(res, vec!['3', '3', '3', '2']);
    /// ```
    fn snoc<U>(self, other: U) -> Snoc<Self, U::Parser>
    where
        Self: Parser<S, Target = Vec<U::Target>> + Sized,
        U: IntoParser<S>,
    {
        Snoc::new(self, other.into_parser())
    }

    /// Parse in sequence, and collect the result.
    /// # Example
    /// ```
    /// use psc::{char, Parser, ParseState};
    ///
    /// let parser = char('3').many().chain(char('2').some());
    /// // (3*)(2+)
    ///
    /// let res = parser.parse(&mut "33322".chars()).unwrap();
    /// assert_eq!(res, vec!['3', '3', '3', '2', '2']);
    /// ```
    fn chain<T, U>(self, other: U) -> Chain<Self, U::Parser>
    where
        Self: Parser<S, Target = Vec<T>> + Sized,
        U: IntoParser<S, Target = Vec<T>>,
    {
        Chain::new(self, other.into_parser())
    }

    /// Kleene Closure Combinator
    /// `p.many()` applies the parser `p` zero or more times.
    /// Returns a vec of the returned values of `p`.
    fn many(self) -> Many<Self>
    where
        Self: Sized,
    {
        Many::new(self)
    }

    /// Kleene Closure Combinator
    /// `p.many_()` applies the parser `p` zero or more times, skipping its result.
    fn many_(self) -> Many_<Self>
    where
        Self: Sized,
    {
        Many_::new(self)
    }

    /// `p.many()` applies the parser `p` one or more times.
    /// Returns a vec of the returned values of `p`.
    fn some(self) -> Some<Self>
    where
        Self: Sized,
    {
        Some::new(self)
    }

    /// `p.many_()` applies the parser `p` one or more times, skipping its result.
    fn some_(self) -> Some_<Self>
    where
        Self: Sized,
    {
        Some_::new(self)
    }

    /// `p.tries()` applies the parser `p` zero or one times.
    fn tries(self) -> Try<Self>
    where
        Self: Sized,
    {
        Try::new(self)
    }

    fn wrap(self) -> ParserWrapper<S, Self>
    where
        Self: Sized,
    {
        ParserWrapper::from(self)
    }

    /// Flat the parser which is constraint as Parser<S, Target=Parser<S>>
    /// It can be defined as `pp.join() ~ pp.and_then(|x| x)`
    fn join(self) -> Join<Self>
    where
        Self: Sized,
        Self::Target: IntoParser<S>,
    {
        Join::new(self)
    }

    fn expected(self, msg: String) -> Or<Self, Failure<S, Self::Target>>
    where
        Self: Sized,
    {
        self.or(Failure::new(ParseMsg::Except(msg)))
    }

    fn unexpected(self, msg: String) -> Or<Self, Failure<S, Self::Target>>
    where
        Self: Sized,
    {
        self.or(Failure::new(ParseMsg::UnExcept(msg)))
    }

    fn info(self, msg: String) -> Or<Self, Failure<S, Self::Target>>
    where
        Self: Sized,
    {
        self.or(Failure::new(ParseMsg::Info(msg)))
    }
}

impl<S: Stream, P: Parser<S>> ParserExt<S> for P {}
