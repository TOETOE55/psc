use crate::adaptor::*;
use crate::covert::IntoParser;
use crate::{Msg, Parser};

pub trait ParserExt<S>: Parser<S> {
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
    /// use psc::{char, Parser, ParserExt, ParseState, ParseLogger};
    ///
    /// let parser = char('+').or(char('-').or(char('*'))).or(char('/'));
    /// // '+' | ('-' | '*') | '/'
    ///
    /// let mut src = ParseState::new("+123");
    /// let mut logger = ParseLogger::default();
    /// let res = parser.parse(&mut src, &mut logger).unwrap();
    /// assert_eq!(res, '+');
    /// assert_eq!(src.as_str(), "123");
    ///
    /// let mut src = ParseState::new("-123");
    /// let mut logger = ParseLogger::default();
    /// let res = parser.parse(&mut src, &mut logger).unwrap();
    /// assert_eq!(res, '-');
    /// assert_eq!(src.as_str(), "123");
    ///
    /// let mut src = ParseState::new("*123");
    /// let mut logger = ParseLogger::default();
    /// let res = parser.parse(&mut src, &mut logger).unwrap();
    /// assert_eq!(res, '*');
    /// assert_eq!(src.as_str(), "123");
    ///
    /// let mut src = ParseState::new("/123");
    /// let mut logger = ParseLogger::default();
    /// let res = parser.parse(&mut src, &mut logger).unwrap();
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
    /// use psc::{char, ParserExt, ParseState, Parser, ParseLogger};
    ///
    /// let parser = char('a').and_r(char('b').and_r(char('c'))).and_r(char('d'));
    /// // a(bc)d
    ///
    /// let mut src = ParseState::new("abcde");
    /// let mut logger = ParseLogger::default();
    /// let res = parser.parse(&mut src, &mut logger).unwrap();
    /// assert_eq!(res, 'd');
    /// assert_eq!(src.as_str(), "e");
    ///
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
    /// use psc::{char, Parser, ParserExt, ParseLogger, ParseState};
    ///
    /// let parser = char('a').and_r(char('b').and_l(char('c'))).and_l(char('d'));
    /// // a(bc)d
    ///
    /// let mut src = ParseState::new("abcde");
    /// let mut logger = ParseLogger::default();
    /// let res = parser.parse(&mut src, &mut logger).unwrap();
    /// assert_eq!(res, 'b');
    /// assert_eq!(src.as_str(), "e");
    ///
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
    /// use psc::{satisfy, Parser, ParserExt, ParseState, ParseLogger};
    /// let parser = satisfy(|ch: &char| ch.is_numeric())
    ///     .map(|c: char| c.to_digit(10))
    ///     .map(Option::unwrap);
    ///
    /// let mut src = ParseState::new("1abc");
    /// let mut logger = ParseLogger::default();
    /// let res = parser.parse(&mut src, &mut logger).unwrap();
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
    /// use psc::{char, satisfy, Parser, ParserExt, ParseState, ParseLogger};
    ///
    /// let pa = char('1').map(|c: char| c.to_digit(10)).map(Option::unwrap);
    /// let pb = satisfy(|ch: &char| ch.is_numeric()).map(|c: char| c.to_digit(10)).map(Option::unwrap);
    /// let parser = pa.map2(pb, |a, b| a+b);
    /// // 1[0-9]
    ///
    /// let res = parser.parse(&mut ParseState::new("123"), &mut ParseLogger::default()).unwrap();
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

    fn map3<U, V, B, F>(self, other: U, another: V, f: F) -> Map3<Self, U::Parser, V::Parser, F>
    where
        Self: Sized,
        U: IntoParser<S>,
        V: IntoParser<S>,
        F: Fn(Self::Target, U::Target, V::Target) -> B,
    {
        Map3::new(self, other.into_parser(), another.into_parser(), f)
    }

    fn filter<F>(self, f: F) -> Filter<Self, F>
    where
        Self: Sized,
        F: Fn(&Self::Target) -> bool,
    {
        Filter::new(self, f)
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
    /// use psc::{satisfy, Parser, char, ParserExt, ParseState, ParseLogger};
    ///
    /// let parser = satisfy(|_| true)
    ///     .and_then(|upper: char| if upper.is_uppercase() {
    ///         char('1')
    ///     } else {
    ///         char('2')
    ///     });
    /// // [A-Z]1 | [a-z]2
    ///
    /// let res = parser.parse(&mut ParseState::new("H1"), &mut ParseLogger::default()).unwrap();
    /// assert_eq!(res, '1');
    ///
    /// let res = parser.parse(&mut ParseState::new("h2"), &mut ParseLogger::default()).unwrap();
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
    /// use psc::{char, Parser, ParseState, ParserExt, ParseLogger};
    ///
    /// let parser = char('3').many().chain(char('2').some());
    /// // (3*)(2+)
    ///
    /// let res = parser.parse(&mut ParseState::new("33322"), &mut ParseLogger::default()).unwrap();
    /// assert_eq!(res.collect::<Vec<_>>(), vec!['3', '3', '3', '2', '2']);
    /// ```
    fn chain<U>(self, other: U) -> Chain<Self, U::Parser>
    where
        Self: Parser<S> + Sized,
        U: IntoParser<S>,
        Self::Target: IntoIterator,
        U::Target: IntoIterator<Item = <Self::Target as IntoIterator>::Item>,
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
    fn option(self) -> Optional<Self>
    where
        Self: Sized,
    {
        Optional::new(self)
    }

    /// Flat the parser which is constraint as Parser<S, Target=Parser<S>>
    /// It can be defined as `pp.join() ~ pp.and_then(|x| x)`
    fn flatten(self) -> Flatten<Self>
    where
        Self: Sized,
        Self::Target: IntoParser<S>,
    {
        Flatten::new(self)
    }

    fn info(self, msg: &str) -> Logger<Self>
    where
        Self: Sized,
    {
        Logger::new(self, Msg::Info(msg.to_owned()))
    }

    fn warn(self, msg: &str) -> Logger<Self>
    where
        Self: Sized,
    {
        Logger::new(self, Msg::Warn(msg.to_owned()))
    }

    fn err(self, msg: &str) -> Logger<Self>
    where
        Self: Sized,
    {
        Logger::new(self, Msg::Err(msg.to_owned()))
    }
}

impl<S, P: Parser<S>> ParserExt<S> for P {}
