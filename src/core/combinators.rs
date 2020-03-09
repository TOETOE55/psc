pub mod common;
pub mod ops;

use crate::core::err::ParseMsg;
use crate::core::traits::parser::Parser;
use crate::core::traits::stream::Stream;
use crate::covert::IntoParser;
use std::marker::PhantomData;
use std::rc::Rc;

/// Pure Combinator
#[derive(Clone)]
pub struct Pure<S, F> {
    x: F,
    _s: PhantomData<fn(&mut S)>,
}

impl<S, F> Pure<S, F> {
    pub fn new(x: F) -> Self {
        Self { x, _s: PhantomData }
    }
}

impl<S: Stream, T, F: Fn() -> T> Parser<S> for Pure<S, F> {
    type Target = T;
    fn parse(&self, _stream: &mut S) -> Result<Self::Target, ParseMsg> {
        Ok((self.x)())
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
    _s: PhantomData<fn(&mut S)>,
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
    fn parse(&self, _stream: &mut S) -> Result<Self::Target, ParseMsg> {
        Err(self.msg.clone())
    }
}

/// Create the failure combinator.
/// It consume an stream and returning Err(msg).
pub fn failure<S, T>(msg: ParseMsg) -> Failure<S, T> {
    Failure::new(msg)
}

/// Fixed-point Combinator
/// To deal with recursion syntax.
pub struct Fix<'a, S: Stream, A> {
    fix: Rc<dyn for<'f> Fn(&'f Self) -> Box<dyn Parser<S, Target = A> + 'f> + 'a>,
}

impl<'a, S: Stream, A> Fix<'a, S, A> {
    pub fn new<F>(fix: F) -> Self
    where
        F: for<'f> Fn(&'f Self) -> Box<dyn Parser<S, Target = A> + 'f> + 'a,
    {
        Self { fix: Rc::new(fix) }
    }

    /// use to make rustc happy.
    /// # Example
    /// ```
    /// use psc::core::combinators::Fix;
    /// use psc::{fix, Parser, char};
    /// use psc::core::traits::parser::ParserExt;
    /// let f = Fix::coerce(|it| Box::new(
    ///            char('1').and_r(it).or(char('0'))));
    /// let parser = fix(Box::new(f));
    ///
    /// let res = parser.parse(&mut "1110".chars()).unwrap();
    /// assert_eq!(res, '0');
    /// ```
    pub fn coerce<F>(f: F) -> F
    where
        F: for<'f> Fn(&'f Self) -> Box<dyn Parser<S, Target = A> + 'f> + 'a,
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        (self.fix)(self).parse(stream)
    }
}

/// Create an fixed-point combinator.
/// # Example
/// ```
/// use psc::{fix, Parser, char};
/// use psc::core::traits::parser::ParserExt;
/// let parser = fix(|it| Box::new(
///         char('1').and_r(it).or(char('0'))));
/// // parser = '1' parser | '0'
///
/// let res = parser.parse(&mut "1110".chars()).unwrap();
/// assert_eq!(res, '0');
/// ```
pub fn fix<'a, S: Stream, A, F>(fix: F) -> Fix<'a, S, A>
where
    F: for<'f> Fn(&'f Fix<'a, S, A>) -> Box<dyn Parser<S, Target = A> + 'f> + 'a,
{
    Fix::new(fix)
}

/// EOF Combinator
pub struct EOF<S>(PhantomData<fn(&mut S)>);
impl<S> EOF<S> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}
impl<S: Stream> Parser<S> for EOF<S> {
    type Target = ();
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        match stream.next() {
            None => Ok(()),
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        let tmp = stream.clone();
        let fst = self.a.parse(stream);
        match fst {
            Err(_) => {
                *stream = tmp;
                self.b.parse(stream)
            }
            ok => ok,
        }
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        self.a.parse(stream)?;
        self.b.parse(stream)
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        let fst = self.a.parse(stream)?;
        self.b.parse(stream)?;
        Ok(fst)
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        self.parser.parse(stream).map(&self.f)
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        let a = self.a.parse(stream)?;
        let b = self.b.parse(stream)?;
        Ok((self.f)(a, b))
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        let f = self.ab.parse(stream)?;
        let a = self.a.parse(stream)?;
        Ok(f(a))
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
    B: IntoParser<S>,
    F: Fn(A::Target) -> B,
{
    type Target = B::Target;
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        let a = self.parser.parse(stream)?;
        (self.f)(a).into_parser().parse(stream)
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        let x = self.a.parse(stream)?;
        let mut xs = self.b.parse(stream)?;
        xs.insert(0, x);
        Ok(xs)
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        let mut xs = self.a.parse(stream)?;
        let x = self.b.parse(stream)?;
        xs.push(x);
        Ok(xs)
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        let mut xs = self.a.parse(stream)?;
        let mut ys = self.b.parse(stream)?;
        xs.append(&mut ys);
        Ok(xs)
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        let mut vec = vec![];
        let mut tmp = stream.clone();
        while let Ok(a) = self.parser.parse(stream) {
            vec.push(a);
            tmp = stream.clone();
        }
        *stream = tmp;
        Ok(vec)
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        let mut tmp = stream.clone();
        while let Ok(_) = self.parser.parse(stream) {
            tmp = stream.clone();
        }
        *stream = tmp;
        Ok(())
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        let mut vec = vec![];

        vec.push(self.parser.parse(stream)?);

        let mut tmp = stream.clone();
        while let Ok(a) = self.parser.parse(stream) {
            vec.push(a);
            tmp = stream.clone();
        }
        *stream = tmp;
        Ok(vec)
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        self.parser.parse(stream)?;
        let mut tmp = stream.clone();
        while let Ok(_) = self.parser.parse(stream) {
            tmp = stream.clone();
        }
        *stream = tmp;
        Ok(())
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        let tmp = stream.clone();
        let tst = self.parser.parse(stream);
        match tst {
            Err(_) => {
                *stream = tmp;
                Ok(None)
            }
            ok => ok.map(Some),
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
    P::Target: IntoParser<S>,
{
    type Target = <<P as Parser<S>>::Target as IntoParser<S>>::Target;
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        self.pp.parse(stream)?.into_parser().parse(stream)
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
    fn parse(&self, stream: &mut S) -> Result<Self::Target, ParseMsg> {
        for p in self.ps.iter() {
            let tmp = stream.clone();
            if let ok @ Ok(_) = p.parse(stream) {
                return ok;
            }
            *stream = tmp;
        }
        Err(ParseMsg::Info("choice nothing.".to_string()))
    }
}
