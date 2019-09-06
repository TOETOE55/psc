pub mod common;

use crate::core::err::ParseMsg;
use crate::core::traits::parser::Parser;
use crate::core::traits::stream::Stream;
use std::marker::PhantomData;
use std::rc::Rc;

/// Pure Combinator
#[derive(Clone)]
pub struct Pure<S, F> {
    x: F,
    _s: PhantomData<S>,
}

impl<S, F> Pure<S, F> {
    pub fn new(x: F) -> Self {
        Self { x, _s: PhantomData }
    }
}

impl<S: Stream, T, F: Fn() -> T> Parser<S> for Pure<S, F> {
    type Target = T;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        Ok(((self.x)(), stream))
    }
}

/// Create an Pure Combinator.
/// The parser will not consume anything, but lift an value to a parser.
pub fn pure<S, T, F: Fn() -> T>(x: F) -> Pure<S, F> {
    Pure::new(x)
}

/// Failure Combinator
pub struct Failure<S, T> {
    msg: ParseMsg,
    _p: PhantomData<T>,
    _s: PhantomData<S>,
}

impl<S, T> Failure<S, T> {
    pub fn new(msg: ParseMsg) -> Self {
        Self {
            msg,
            _p: PhantomData,
            _s: PhantomData,
        }
    }
}

impl<S: Stream, T> Parser<S> for Failure<S, T> {
    type Target = T;
    fn parse(&self, _stream: S) -> Result<(Self::Target, S), ParseMsg> {
        Err(self.msg.clone())
    }
}

/// Create the failure combinator.
/// It consume an stream and returning Err(msg).
pub fn failure<S, T>(msg: ParseMsg) -> Failure<S, T> {
    Failure::new(msg)
}

/// Fixed-point Combinator
/// To deal with some recursion syntax.
pub struct Fix<'a, S: Stream, A> {
    fix: Rc<dyn for<'f> Fn(&'f Fix<S, A>) -> Box<dyn Parser<S, Target = A> + 'f> + 'a>,
}

impl<'a, S: Stream, A> Fix<'a, S, A> {
    pub fn new<F>(fix: F) -> Self
    where
        F: for<'f> Fn(&'f Fix<S, A>) -> Box<dyn Parser<S, Target = A> + 'f> + 'a,
    {
        Self { fix: Rc::new(fix) }
    }

    /// use to make rustc happy.
    /// # Example
    /// ```
    /// use psc::core::combinators::Fix;
    /// use psc::{fix, Parser};
    /// let f = Fix::coerce(|it| Box::new(
    ///            char('1').and_r(it).or(char('0'))));
    /// let parser = fix(Box::new(f));
    ///
    /// let (res, _) = parser.parse("1110")?;
    /// assert_eq!(res, '0');
    /// ```
    pub fn coerce<F>(f: F) -> F
    where
        F: for<'f> Fn(&'f Fix<S, A>) -> Box<dyn Parser<S, Target = A> + 'f> + 'a,
    {
        f
    }
}

impl<'a, S: Stream, A> Clone for Fix<'a, S, A> {
    fn clone(&self) -> Self {
        Fix {
            fix: self.fix.clone(),
        }
    }
}

impl<'a, S: Stream, A> Parser<S> for Fix<'a, S, A> {
    type Target = A;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        (self.fix)(self).parse(stream)
    }
}

/// Create an fixed-point combinator.
/// # Example
/// ```
/// use psc::{fix, Parser};
/// let parser = fix(|it| Box::new(
///         char('1').and_r(it).or(char('0'))));
/// // parser = '1' parser | '0'
///
/// let (res, _) = parser.parse("1110")?;
/// assert_eq!(res, '0');
/// ```
pub fn fix<'a, S: Stream, A, F>(fix: F) -> Fix<'a, S, A>
where
    F: for<'f> Fn(&'f Fix<S, A>) -> Box<dyn Parser<S, Target = A> + 'f> + 'a,
{
    Fix::new(fix)
}

/// EOF Combinator
pub struct EOF<S>(PhantomData<S>);
impl<S> EOF<S> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}
impl<S: Stream> Parser<S> for EOF<S> {
    type Target = ();
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        match stream.next() {
            None => Ok(((), Stream::empty())),
            _ => Err(ParseMsg::Except("expected eof.".to_string())),
        }
    }
}
pub fn eof<S: Stream>() -> EOF<S> {
    EOF::new()
}

/// Alternative Combinator
#[derive(Clone)]
pub struct Or<A, B> {
    a: A,
    b: B,
}

impl<A, B> Or<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<S: Stream + Clone, A, B> Parser<S> for Or<A, B>
where
    A: Parser<S>,
    B: Parser<S, Target = A::Target>,
{
    type Target = A::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        self.a
            .parse(stream.clone())
            .or_else(|_| self.b.parse(stream))
    }
}

/// Sequence Combinator
#[derive(Clone)]
pub struct AndR<A, B> {
    a: A,
    b: B,
}

impl<A, B> AndR<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<S: Stream, A: Parser<S>, B: Parser<S>> Parser<S> for AndR<A, B> {
    type Target = B::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (_, stream) = self.a.parse(stream)?;
        let (b, stream) = self.b.parse(stream)?;
        Ok((b, stream))
    }
}

/// Sequence Combinator
#[derive(Clone)]
pub struct AndL<A, B> {
    a: A,
    b: B,
}

impl<A, B> AndL<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<S: Stream, A: Parser<S>, B: Parser<S>> Parser<S> for AndL<A, B> {
    type Target = A::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (a, stream) = self.a.parse(stream)?;
        let (_, stream) = self.b.parse(stream)?;
        Ok((a, stream))
    }
}

/// Map Combinator
#[derive(Clone)]
pub struct Map<P, F> {
    parser: P,
    f: F,
}

impl<P, F> Map<P, F> {
    pub fn new(parser: P, f: F) -> Self {
        Self { parser, f }
    }
}

impl<S: Stream, B, P: Parser<S>, F> Parser<S> for Map<P, F>
where
    F: Fn(P::Target) -> B,
{
    type Target = B;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (res, stream) = self.parser.parse(stream)?;
        Ok(((self.f)(res), stream))
    }
}

/// Map2 Combinator
#[derive(Clone)]
pub struct Map2<A, B, F> {
    a: A,
    b: B,
    f: F,
}

impl<A, B, F> Map2<A, B, F> {
    pub fn new(a: A, b: B, f: F) -> Self {
        Self { a, b, f }
    }
}

impl<S: Stream, A, B, T, F> Parser<S> for Map2<A, B, F>
where
    A: Parser<S>,
    B: Parser<S>,
    F: Fn(A::Target, B::Target) -> T,
{
    type Target = T;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (a, stream) = self.a.parse(stream)?;
        let (b, stream) = self.b.parse(stream)?;
        Ok(((self.f)(a, b), stream))
    }
}

/// Applicative Combinator
#[derive(Clone)]
pub struct App<AB, A> {
    ab: AB,
    a: A,
}

impl<AB, A> App<AB, A> {
    pub fn new(ab: AB, a: A) -> Self {
        Self { ab, a }
    }
}

impl<S: Stream, AB, A, T, F> Parser<S> for App<AB, A>
where
    A: Parser<S>,
    AB: Parser<S, Target = F>,
    F: Fn(A::Target) -> T,
{
    type Target = T;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (f, stream) = self.ab.parse(stream)?;
        let (a, stream) = self.a.parse(stream)?;
        Ok((f(a), stream))
    }
}

/// Context Sensitive Sequence Combinator
#[derive(Clone)]
pub struct AndThen<P, F> {
    parser: P,
    f: F,
}

impl<P, F> AndThen<P, F> {
    pub fn new(parser: P, f: F) -> Self {
        Self { parser, f }
    }
}

impl<S: Stream, A, B, F> Parser<S> for AndThen<A, F>
where
    A: Parser<S>,
    B: Parser<S>,
    F: Fn(A::Target) -> B,
{
    type Target = B::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (a, stream) = self.parser.parse(stream)?;
        (self.f)(a).parse(stream)
    }
}

/// Sequence Combinator
#[derive(Clone)]
pub struct Cons<A, B> {
    a: A,
    b: B,
}

impl<A, B> Cons<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<S: Stream, A, B> Parser<S> for Cons<A, B>
where
    A: Parser<S>,
    B: Parser<S, Target = Vec<A::Target>>,
{
    type Target = Vec<A::Target>;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (x, stream) = self.a.parse(stream)?;
        let (mut xs, stream) = self.b.parse(stream)?;
        xs.insert(0, x);
        Ok((xs, stream))
    }
}

/// Sequence Combinator
#[derive(Clone)]
pub struct Snoc<A, B> {
    a: A,
    b: B,
}

impl<A, B> Snoc<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<S: Stream, A, B> Parser<S> for Snoc<A, B>
where
    A: Parser<S, Target = Vec<B::Target>>,
    B: Parser<S>,
{
    type Target = Vec<B::Target>;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (mut xs, stream) = self.a.parse(stream)?;
        let (x, stream) = self.b.parse(stream)?;
        xs.push(x);
        Ok((xs, stream))
    }
}

/// Sequence Combinator
#[derive(Clone)]
pub struct Chain<A, B> {
    a: A,
    b: B,
}

impl<A, B> Chain<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<S: Stream, T, A, B> Parser<S> for Chain<A, B>
where
    A: Parser<S, Target = Vec<T>>,
    B: Parser<S, Target = Vec<T>>,
{
    type Target = Vec<T>;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (mut xs, stream) = self.a.parse(stream)?;
        let (mut ys, stream) = self.b.parse(stream)?;
        xs.append(&mut ys);
        Ok((xs, stream))
    }
}

/// Kleene Closure Combinator
#[derive(Clone)]
pub struct Many<P> {
    parser: P,
}

impl<P> Many<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S: Stream + Clone, P: Parser<S>> Parser<S> for Many<P> {
    type Target = Vec<P::Target>;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let mut vec = vec![];
        let mut stream = stream;
        while let Ok((a, s)) = self.parser.parse(stream.clone()) {
            vec.push(a);
            stream = s;
        }

        Ok((vec, stream))
    }
}

/// Kleene Closure Combinator
#[derive(Clone)]
pub struct Many_<P> {
    parser: P,
}

impl<P> Many_<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S: Stream + Clone, P: Parser<S>> Parser<S> for Many_<P> {
    type Target = ();
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let mut stream = stream;
        while let Ok((_, s)) = self.parser.parse(stream.clone()) {
            stream = s;
        }
        Ok(((), stream))
    }
}

/// Some Combinator
#[derive(Clone)]
pub struct Some<P> {
    parser: P,
}

impl<P> Some<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S: Stream + Clone, P: Parser<S>> Parser<S> for Some<P> {
    type Target = Vec<P::Target>;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let mut vec = vec![];

        let (a, mut stream) = self.parser.parse(stream)?;
        vec.push(a);

        while let Ok((a, s)) = self.parser.parse(stream.clone()) {
            vec.push(a);
            stream = s;
        }

        Ok((vec, stream))
    }
}

/// Some Combinator
#[derive(Clone)]
pub struct Some_<P> {
    parser: P,
}

impl<P> Some_<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S: Stream + Clone, P: Parser<S>> Parser<S> for Some_<P> {
    type Target = ();
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (_, mut stream) = self.parser.parse(stream)?;
        while let Ok((_, s)) = self.parser.parse(stream.clone()) {
            stream = s;
        }
        Ok(((), stream))
    }
}

/// Try Combinator
#[derive(Clone)]
pub struct Try<P> {
    parser: P,
}

impl<P> Try<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S: Stream + Clone, P: Parser<S>> Parser<S> for Try<P> {
    type Target = Option<P::Target>;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        match self.parser.parse(stream.clone()) {
            Ok((a, stream)) => Ok((Some(a), stream)),
            _ => Ok((None, stream)),
        }
    }
}

/// Join Combinator
pub struct Join<P> {
    pp: P,
}

impl<P> Join<P> {
    pub fn new(pp: P) -> Self {
        Self { pp }
    }
}

impl<S: Stream, P> Parser<S> for Join<P>
where
    P: Parser<S>,
    P::Target: Parser<S>,
{
    type Target = <<P as Parser<S>>::Target as Parser<S>>::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (p, stream) = self.pp.parse(stream)?;
        p.parse(stream)
    }
}

/// Multiple Choice Combinator
pub struct Choice<S, A> {
    ps: Vec<Box<dyn Parser<S, Target = A>>>,
    _s: PhantomData<S>,
}

impl<S, A> Choice<S, A> {
    pub fn new(ps: Vec<Box<dyn Parser<S, Target = A>>>) -> Self {
        Self {
            ps,
            _s: PhantomData,
        }
    }
}

pub fn choice<S: Stream + Clone, A>(ps: Vec<Box<dyn Parser<S, Target = A>>>) -> Choice<S, A> {
    Choice::new(ps)
}

impl<S: Stream + Clone, A> Parser<S> for Choice<S, A> {
    type Target = A;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        for p in self.ps.iter() {
            if let ok @ Ok(_) = p.parse(stream.clone()) {
                return ok;
            }
        }
        Err(ParseMsg::Info("choice nothing.".to_string()))
    }
}
