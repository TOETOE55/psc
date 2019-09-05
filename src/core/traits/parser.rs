use crate::core::traits::stream::Stream;
use crate::core::combinators::{Or, AndR, App, Map, AndThen, Many, Some, Failure, AndL, Cons, Snoc, Join, Many_, Some_, Chain, Try, Map2};
use crate::core::err::ParseMsg;
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
    /// let (res, s) = parser.parse("+123")?;
    /// assert_eq!(res, Some('+'));
    /// assert_eq!(s, "123");
    ///
    /// let (res, s) = parser.parse("-123")?;
    /// assert_eq!(res, Some('-'));
    /// assert_eq!(s, "123");
    ///
    /// let (res, s) = parser.parse("123")?;
    /// assert_eq!(res, None);
    /// assert_eq!(s, "123");
    /// ```
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg>;

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
    /// let (res, s) = parser.parse("+123")?;
    /// assert_eq!(res, '+');
    /// assert_eq!(s, "123");
    ///
    /// let (res, s) = parser.parse("-123")?;
    /// assert_eq!(res, '-');
    /// assert_eq!(s, "123");
    ///
    /// let (res, s) = parser.parse("*123")?;
    /// assert_eq!(res, '*');
    /// assert_eq!(s, "123");
    ///
    /// let (res, s) = parser.parse("*123")?;
    /// assert_eq!(res, '/');
    /// assert_eq!(s, "123");
    /// ```
    fn or<P>(self, other: P) -> Or<Self, P> where
        Self: Sized,
        P: Parser<S, Target=Self::Target>,
    {
        Or::new(self, other)
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
    /// let (res, s) = parser.parse("abcde")?;
    /// assert_eq!(res, 'd');
    /// assert_eq!(s, "e");
    ///
    /// let res = parser.parse("ab").ok();
    /// assert_eq!(res, None);
    ///
    /// let res = parser.parse("acde").ok();
    /// assert_eq!(res, None);
    /// ```
    fn and_r<P>(self, other: P) -> AndR<Self, P> where
        Self: Sized,
        P: Parser<S>,
    {
        AndR::new(self, other)
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
    /// let (res, s) = parser.parse("abcde")?;
    /// assert_eq!(res, 'b');
    /// assert_eq!(s, "e");
    ///
    /// let res = parser.parse("ab").ok();
    /// assert_eq!(res, None);
    ///
    /// let res = parser.parse("acde").ok();
    /// assert_eq!(res, None);
    /// ```
    fn and_l<P>(self, other: P) -> AndL<Self, P> where
        Self: Sized,
        P: Parser<S>,
    {
        AndL::new(self, other)
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
    ///     .map(char::to_digit)
    ///     .map(Option::unwrap);
    ///
    /// let (res, s) = parser.parse("1abc")?;
    /// assert_eq!(res, 1);
    /// assert_eq!(s, "abc");
    /// ```
    fn map<B, F>(self, f: F) -> Map<Self, F> where
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
    /// let pa = char('1').map(char::to_digit);
    /// let pb = satisfy(|ch: &char| ch.is_numeric()).map(char::to_digit);
    /// let parser = pa.map2(pb, |a, b| a+b);
    /// // 1[0-9]
    ///
    /// let (res, s) = parser.parse("123")?;
    /// assert_eq!(res, 3);
    /// assert_eq!(s, "3");
    /// ```
    fn map2<P, B, F>(self, other: P, f: F) -> Map2<Self, P, F> where
        Self: Sized,
        P: Parser<S>,
        F: Fn(Self::Target, P::Target) -> B,
    {
        Map2::new(self, other, f)
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
    fn app<P, T, F>(self, other: P) -> App<Self, P> where
        Self: Parser<S, Target=F> + Sized,
        P: Parser<S>,
        F: Fn(P::Target) -> T,
    {
        App::new(self, other)
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
    /// use psc::{satisfy, Parser};
    ///
    /// let parser = satisfy(|ch: &char| ch.is_uppercase())
    ///     .and_then(|upper| if upper {
    ///         char('1')
    ///     } else {
    ///         char('2')
    ///     });
    /// // [A-Z]1 | [a-z]2
    ///
    /// let (res, s) = parser.parse("H1");
    /// assert_eq!(res, '1');
    ///
    /// let (res, s) = parser.parse("h2");
    /// assert_eq!(res, '2');
    /// ```
    fn and_then<P, F>(self, f: F) -> AndThen<Self, F> where
        Self: Sized,
        P: Parser<S>,
        F: Fn(Self::Target) -> P,
    {
        AndThen::new(self, f)
    }

    /// Parse in sequence, and collect the result.
    /// # Example
    /// ```
    /// use psc::{char, Parser, ParseState};
    ///
    /// let parser = char('2').cons(char('3')).cons(char('3').many());
    /// // 23(3*)
    ///
    /// let (res, s) = parser.parse(ParseState::new("23334"))?;
    /// assert_eq!(res, vec!['2', '3', '3', '3']);
    /// assert_eq!(s.src, "4");
    /// ```
    fn cons<P>(self, other: P) -> Cons<Self, P> where
        Self: Sized,
        P: Parser<S, Target=Vec<Self::Target>>,
    {
        Cons::new(self, other)
    }

    /// Parse in sequence, and collect the result.
    /// # Example
    /// ```
    /// use psc::{char, Parser, ParseState};
    ///
    /// let parser = char('2').many().snoc(char('3'));
    /// // (3*)2
    ///
    /// let (res, s) = parser.parse("3332")?;
    /// assert_eq!(res, vec!['3', '3', '3', '2']);
    /// assert_eq!(s, "");
    /// ```
    fn snoc<P>(self, other: P) -> Snoc<Self, P> where
        Self: Parser<S, Target=Vec<P::Target>> + Sized,
        P: Parser<S>,
    {
        Snoc::new(self, other)
    }

    /// Parse in sequence, and collect the result.
    /// # Example
    /// ```
    /// use psc::{char, Parser, ParseState};
    ///
    /// let parser = char('2').many().chain(char('3').some());
    /// // (3*)(2+)
    ///
    /// let (res, s) = parser.parse("33322")?;
    /// assert_eq!(res, vec!['3', '3', '3', '2', '2']);
    /// assert_eq!(s, "");
    /// ```
    fn chain<T, P>(self, other: P) -> Chain<Self, P> where
        Self: Parser<S, Target=Vec<T>> + Sized,
        P: Parser<S, Target=Vec<T>>,
    {
        Chain::new(self, other)
    }

    /// Kleene Closure Combinator
    /// `p.many()` applies the parser `p` zero or more times.
    /// Returns a vec of the returned values of `p`.
    fn many(self) -> Many<Self> where Self: Sized {
        Many::new(self)
    }

    /// Kleene Closure Combinator
    /// `p.many_()` applies the parser `p` zero or more times, skipping its result.
    fn many_(self) -> Many_<Self> where Self: Sized {
        Many_::new(self)
    }

    /// `p.many()` applies the parser `p` one or more times.
    /// Returns a vec of the returned values of `p`.
    fn some(self) -> Some<Self> where Self: Sized {
        Some::new(self)
    }

    /// `p.many_()` applies the parser `p` one or more times, skipping its result.
    fn some_(self) -> Some_<Self> where Self: Sized {
        Some_::new(self)
    }

    /// `p.tries()` applies the parser `p` zero or one times.
    fn tries(self) -> Try<Self> where Self: Sized {
        Try::new(self)
    }

    /// Flat the parser which is constraint as Parser<S, Target=Parser<S>>
    /// It can be defined as `pp.join() ~ pp.and_then(|x| x)`
    fn join(self) -> Join<Self> where
        Self: Sized,
        Self::Target: Parser<S>,
    {
        Join::new(self)
    }

    fn label(self, msg: String) -> Or<Self, Failure<S, Self::Target>> where Self: Sized {
        self.or(Failure::new(ParseMsg::Except(msg)))
    }

    fn unexpected(self, msg: String) -> Or<Self, Failure<S, Self::Target>> where Self: Sized {
        self.or(Failure::new(ParseMsg::UnExcept(msg)))
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
