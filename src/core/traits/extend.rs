use crate::core::combinator::adaptor::*;
use crate::covert::IntoParser;
use crate::Parser;

pub trait ParserExt<S, E>: Parser<S, E> {
    /**
    # Map Combinator.
    The parser `p.map(f)` creates an parser which calls the closure `f` on the parse result of p.

    Functor law satisfied:
    1. **identity**: `p.map(|x| x) ~ p`
    2. **composition**: `p.map(f).map(g) ~ p.map(|x| g(f(x))`
    ## Example
    ```
    use psc::*;

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
    use psc::*;

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
    use psc::*;

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
    use psc::*;

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
    use psc::*;

    let parser = char('a').and_r(char('b').and_r('c')).and_r('d');
    // a(bc)d
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
    use psc::*;

    let parser = char('a').and_l(char('b').and_l('c')).and_l('d');
    // a(bc)d
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
    use psc::*;

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
    use psc::*;

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
