pub mod common;

use std::marker::PhantomData;
use crate::core::traits::parser::Parser;
use crate::core::traits::stream::Stream;
use crate::core::err::ParseMsg;
use std::rc::Rc;

///
#[derive(Clone)]
pub struct Pure<S, F> {
    x: F,
    _s: PhantomData<S>,
}

impl<S, F> Pure<S, F> {
    pub fn new(x: F) -> Self {
        Self {
            x,
            _s: PhantomData,
        }
    }
}

impl<S: Stream, T, F: Fn() -> T> Parser<S> for Pure<S, F> {
    type Target = T;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg>  {
        Ok(((self.x)(), stream))
    }
}

pub fn pure<S, T, F: Fn() -> T>(x: F) -> Pure<S, F> {
    Pure::new(x)
}

/// failure
pub struct Failure<S, T>{
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

pub fn failure<S, T>(msg: ParseMsg) -> Failure<S, T> {
    Failure::new(msg)
}

/// fix
pub struct Fix<'a, S: Stream, A> {
    fix: Rc<dyn for<'f> Fn(&'f Fix<S, A>) -> Box<dyn Parser<S, Target=A> + 'f> + 'a>,
}

impl<'a, S: Stream, A> Fix<'a, S, A> {
    pub fn new<F>(fix: F) -> Self where
        F: for<'f> Fn(&'f Fix<S, A>) -> Box<dyn Parser<S, Target=A> + 'f> + 'a,
    {
        Self {
            fix: Rc::new(fix)
        }
    }

    pub fn coerce<F>(f: F) -> F where
        F: for<'f> Fn(&'f Fix<S, A>) -> Box<dyn Parser<S, Target=A> + 'f> + 'a
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

pub fn fix<'a, S: Stream, A, F>(fix: F) -> Fix<'a, S, A> where
    F: for<'f> Fn(&'f Fix<S, A>) -> Box<dyn Parser<S, Target=A> + 'f> + 'a,
{
    Fix::new(fix)
}

/// EOF
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
            _    => Err(ParseMsg::Except("expected eof.".to_string())),
        }
    }
}
pub fn eof<S: Stream>() -> EOF<S> {
    EOF::new()
}

/// or
#[derive(Clone)]
pub struct Or<A, B> {
    a: A,
    b: B,
}

impl<A, B> Or<A, B> {
    pub fn new(a:A, b: B) -> Self {
        Self { a, b }
    }
}

impl<S: Stream + Clone, A, B> Parser<S> for Or<A, B> where
    A: Parser<S>,
    B: Parser<S, Target=A::Target>,
{
    type Target = A::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg>  {
        self.a.parse(stream.clone()).or_else(|_| self.b.parse(stream))
    }
}

/// and
#[derive(Clone)]
pub struct AndR<A, B> {
    a: A,
    b: B,
}

impl<A, B> AndR<A, B> {
    pub fn new(a:A, b: B) -> Self {
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

/// andl
#[derive(Clone)]
pub struct AndL<A, B> {
    a: A,
    b: B,
}

impl<A, B> AndL<A, B> {
    pub fn new(a:A, b: B) -> Self {
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

/// map
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
    where F: Fn(P::Target) -> B,
{
    type Target = B;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg>  {
        let (res, stream) = self.parser.parse(stream)?;
        Ok(((self.f)(res), stream))
    }
}

/// applicative
#[derive(Clone)]
pub struct App<A, AB> {
    a: A,
    ab: AB,
}

impl<A, AB> App<A, AB> {
    pub fn new(a: A, ab: AB) -> Self {
        Self { a, ab }
    }
}

impl<S: Stream, A, AB, T, F> Parser<S> for App<A, AB> where
    A: Parser<S>,
    F: Fn(A::Target) -> T,
    AB: Parser<S, Target=F>,
{
    type Target = T;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg>  {
        let (f, stream) = self.ab.parse(stream)?;
        let (a, stream) = self.a.parse(stream)?;
        Ok((f(a), stream))
    }
}

/// and_then
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

impl<S: Stream, A, B, F> Parser<S> for AndThen<A, F> where
    A: Parser<S>,
    B: Parser<S>,
    F: Fn(A::Target) -> B,
{
    type Target = B::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg>  {
        let (a, stream) = self.parser.parse(stream)?;
        (self.f)(a).parse(stream)
    }
}

pub struct ChainL<A, B> {
    a: A,
    b: B,
}

impl<A, B> ChainL<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<S: Stream, A, B> Parser<S> for ChainL<A, B> where
    A: Parser<S>,
    B: Parser<S, Target=Vec<A::Target>>,
{
    type Target = Vec<A::Target>;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (x , stream) = self.a.parse(stream)?;
        let (mut xs, stream) = self.b.parse(stream)?;
        xs.insert(0, x);
        Ok((xs, stream))
    }
}

pub struct ChainR<A, B> {
    a: A,
    b: B,
}

impl<A, B> ChainR<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<S: Stream, A, B> Parser<S> for ChainR<A, B> where
    A: Parser<S, Target=Vec<B::Target>>,
    B: Parser<S>,
{
    type Target = Vec<B::Target>;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (mut xs , stream) = self.a.parse(stream)?;
        let (x, stream) = self.b.parse(stream)?;
        xs.push(x);
        Ok((xs, stream))
    }
}

/// many
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
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg>  {
        let mut vec = vec![];
        let mut stream = stream;
        loop {
            match self.parser.parse(stream.clone()) {
                Ok((a, s)) => {
                    vec.push(a);
                    stream = s;
                },
                _ => break,
            }
        }

        Ok((vec, stream))
    }
}


/// some
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

        loop {
            match self.parser.parse(stream.clone()) {
                Ok((a, s)) => {
                    vec.push(a);
                    stream = s;
                },
                _ => break,
            }
        }

        Ok((vec, stream))
    }
}

pub struct Join<P> {
    pp: P,
}

impl<P> Join<P> {
    pub fn new(pp: P) -> Self {
        Self {
            pp
        }
    }
}

impl<S: Stream, P> Parser<S> for Join<P> where
    P: Parser<S>,
    P::Target: Parser<S>,
{
    type Target = <<P as Parser<S>>::Target as Parser<S>>::Target;
    fn parse(&self, stream: S) -> Result<(Self::Target, S), ParseMsg> {
        let (p, stream) = self.pp.parse(stream)?;
        p.parse(stream)
    }
}