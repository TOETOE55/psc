use crate::core::traits::covert::IntoParser;
use crate::{Msg, ParseLogger, Parser};
use std::marker::PhantomData;
use std::rc::Rc;

/// Pure Combinator
#[derive(Debug, Copy, Clone)]
pub struct Pure<F> {
    x: F,
}

impl<F> Pure<F> {
    pub fn new(x: F) -> Self {
        Self { x }
    }
}

impl<S, T, F: Fn() -> T> Parser<S> for Pure<F> {
    type Target = T;

    fn parse(&self, _: &mut S, _: &mut ParseLogger) -> Option<Self::Target> {
        Some((self.x)())
    }
}

/// Create an Pure Combinator.
/// The parser will not consume anything, but lift an value to a parser.
pub fn pure<T, F: Fn() -> T>(x: F) -> Pure<F> {
    Pure::new(x)
}

/// Failure Combinator
#[derive(Debug)]
pub struct Empty<T> {
    _marker: PhantomData<fn() -> Option<T>>,
}

impl<T> Empty<T> {
    pub fn new() -> Self {
        Self {
            _marker: PhantomData,
        }
    }
}

impl<T> Default for Empty<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Clone for Empty<T> {
    fn clone(&self) -> Self {
        Self::default()
    }
}

impl<T> Copy for Empty<T> {}

impl<S, T> Parser<S> for Empty<T> {
    type Target = T;

    fn parse(&self, _: &mut S, _: &mut ParseLogger) -> Option<Self::Target> {
        None
    }
}

/// Create the failure combinator.
/// It consume an stream and returning Err(msg).
pub fn empty<T>() -> Empty<T> {
    Empty::new()
}

/// Fixed-point Combinator
/// To deal with recursion syntax.
pub struct Fix<'a, S, A> {
    fix: Rc<dyn for<'f> Fn(&'f Self) -> Box<dyn Parser<S, Target = A> + 'f> + 'a>,
}

impl<'a, S, A> Fix<'a, S, A> {
    pub fn new<F>(fix: F) -> Self
    where
        F: for<'f> Fn(&'f Self) -> Box<dyn Parser<S, Target = A> + 'f> + 'a,
    {
        Self { fix: Rc::new(fix) }
    }

    /// use to make rustc happy.
    pub fn coerce<F>(f: F) -> F
    where
        F: for<'f> Fn(&'f Self) -> Box<dyn Parser<S, Target = A> + 'f> + 'a,
    {
        f
    }
}

impl<'a, S, A> Clone for Fix<'a, S, A> {
    fn clone(&self) -> Self {
        Fix {
            fix: self.fix.clone(),
        }
    }
}

impl<'a, S, A> Parser<S> for Fix<'a, S, A> {
    type Target = A;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        (self.fix)(self).parse(stream, logger)
    }
}

/// Create an fixed-point combinator.
/// # Example
/// ```
/// use psc::{fix, Parser, char, ParserExt, ParseState, ParseLogger};
/// let parser = fix(|it| Box::new(
///         char('1').and_r(it).or(char('0'))));
/// // parser = '1' parser | '0'
///
/// let res = parser.parse(&mut ParseState::new("1110"), &mut ParseLogger::new()).unwrap();
/// assert_eq!(res, '0');
/// ```
pub fn fix<'a, S, A, F>(fix: F) -> Fix<'a, S, A>
where
    F: for<'f> Fn(&'f Fix<'a, S, A>) -> Box<dyn Parser<S, Target = A> + 'f> + 'a,
{
    Fix::new(fix)
}

/// Alternative Combinator
#[derive(Clone, Copy, Debug)]
pub struct Or<PA, PB> {
    pa: PA,
    pb: PB,
}

impl<PA, PB> Or<PA, PB> {
    pub fn new(pa: PA, pb: PB) -> Self {
        Self { pa, pb }
    }
}

impl<S: Clone, PA, PB> Parser<S> for Or<PA, PB>
where
    PA: Parser<S>,
    PB: Parser<S, Target = PA::Target>,
{
    type Target = PA::Target;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        let s0 = stream.clone();
        let l0 = logger.clone();
        let fst = self.pa.parse(stream, logger);
        match fst {
            None => {
                *stream = s0;
                *logger = l0;
                self.pb.parse(stream, logger)
            }
            ok => ok,
        }
    }
}

/// Sequence Combinator
#[derive(Clone, Copy, Debug)]
pub struct AndR<PA, PB> {
    pa: PA,
    pb: PB,
}

impl<PA, PB> AndR<PA, PB> {
    pub fn new(pa: PA, pb: PB) -> Self {
        Self { pa, pb }
    }
}

impl<S, PA: Parser<S>, PB: Parser<S>> Parser<S> for AndR<PA, PB> {
    type Target = PB::Target;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        self.pa.parse(stream, logger)?;
        self.pb.parse(stream, logger)
    }
}

/// Sequence Combinator
#[derive(Clone, Copy, Debug)]
pub struct AndL<PA, PB> {
    pa: PA,
    pb: PB,
}

impl<PA, PB> AndL<PA, PB> {
    pub fn new(pa: PA, pb: PB) -> Self {
        Self { pa, pb }
    }
}

impl<S, PA: Parser<S>, PB: Parser<S>> Parser<S> for AndL<PA, PB> {
    type Target = PA::Target;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        let fst = self.pa.parse(stream, logger)?;
        self.pb.parse(stream, logger)?;
        Some(fst)
    }
}

/// Map Combinator
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

impl<S, B, P: Parser<S>, F> Parser<S> for Map<P, F>
where
    F: Fn(P::Target) -> B,
{
    type Target = B;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        self.parser.parse(stream, logger).map(&self.f)
    }
}

/// Map2 Combinator
#[derive(Clone, Copy)]
pub struct Map2<PA, PB, F> {
    pa: PA,
    pb: PB,
    f: F,
}

impl<PA, PB, F> Map2<PA, PB, F> {
    pub fn new(pa: PA, pb: PB, f: F) -> Self {
        Self { pa, pb, f }
    }
}

impl<S, PA, PB, T, F> Parser<S> for Map2<PA, PB, F>
where
    PA: Parser<S>,
    PB: Parser<S>,
    F: Fn(PA::Target, PB::Target) -> T,
{
    type Target = T;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        let a = self.pa.parse(stream, logger)?;
        let b = self.pb.parse(stream, logger)?;
        Some((self.f)(a, b))
    }
}

/// Map3 Combinator
#[derive(Clone, Copy)]
pub struct Map3<PA, PB, PC, F> {
    pa: PA,
    pb: PB,
    pc: PC,
    f: F,
}

impl<PA, PB, PC, F> Map3<PA, PB, PC, F> {
    pub fn new(pa: PA, pb: PB, pc: PC, f: F) -> Self {
        Self { pa, pb, pc, f }
    }
}

impl<S, PA, PB, PC, T, F> Parser<S> for Map3<PA, PB, PC, F>
where
    PA: Parser<S>,
    PB: Parser<S>,
    PC: Parser<S>,
    F: Fn(PA::Target, PB::Target, PC::Target) -> T,
{
    type Target = T;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        let a = self.pa.parse(stream, logger)?;
        let b = self.pb.parse(stream, logger)?;
        let c = self.pc.parse(stream, logger)?;
        Some((self.f)(a, b, c))
    }
}

/// Map2 Combinator
#[derive(Clone, Copy)]
pub struct Map4<PA, PB, PC, PD, F> {
    pa: PA,
    pb: PB,
    pc: PC,
    pd: PD,
    f: F,
}

impl<PA, PB, PC, PD, F> Map4<PA, PB, PC, PD, F> {
    pub fn new(pa: PA, pb: PB, pc: PC, pd: PD, f: F) -> Self {
        Self { pa, pb, pc, pd, f }
    }
}

impl<S, PA, PB, PC, PD, T, F> Parser<S> for Map4<PA, PB, PC, PD, F>
where
    PA: Parser<S>,
    PB: Parser<S>,
    PC: Parser<S>,
    PD: Parser<S>,
    F: Fn(PA::Target, PB::Target, PC::Target, PD::Target) -> T,
{
    type Target = T;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        let a = self.pa.parse(stream, logger)?;
        let b = self.pb.parse(stream, logger)?;
        let c = self.pc.parse(stream, logger)?;
        let d = self.pd.parse(stream, logger)?;
        Some((self.f)(a, b, c, d))
    }
}

/// Map Combinator
#[derive(Clone, Copy)]
pub struct Filter<P, F> {
    parser: P,
    f: F,
}

impl<P, F> Filter<P, F> {
    pub fn new(parser: P, f: F) -> Self {
        Self { parser, f }
    }
}

impl<S, P: Parser<S>, F> Parser<S> for Filter<P, F>
where
    F: Fn(&P::Target) -> bool,
{
    type Target = P::Target;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        self.parser.parse(stream, logger).filter(&self.f)
    }
}

/// Context Sensitive Sequence Combinator
#[derive(Copy, Clone)]
pub struct AndThen<P, F> {
    parser: P,
    f: F,
}

impl<P, F> AndThen<P, F> {
    pub fn new(parser: P, f: F) -> Self {
        Self { parser, f }
    }
}

impl<S, PA, PB, F> Parser<S> for AndThen<PA, F>
where
    PA: Parser<S>,
    PB: IntoParser<S>,
    F: Fn(PA::Target) -> PB,
{
    type Target = PB::Target;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        let a = self.parser.parse(stream, logger)?;
        (self.f)(a).into_parser().parse(stream, logger)
    }
}

/// Sequence Combinator
#[derive(Copy, Clone)]
pub struct Chain<PA, PB> {
    pa: PA,
    pb: PB,
}

impl<PA, PB> Chain<PA, PB> {
    pub fn new(pa: PA, pb: PB) -> Self {
        Self { pa, pb }
    }
}

impl<S, PA, PB> Parser<S> for Chain<PA, PB>
where
    PA: Parser<S>,
    PB: Parser<S>,
    PA::Target: IntoIterator,
    PB::Target: IntoIterator<Item = <<PA as Parser<S>>::Target as IntoIterator>::Item>,
{
    type Target = std::iter::Chain<
        <<PA as Parser<S>>::Target as IntoIterator>::IntoIter,
        <<PB as Parser<S>>::Target as IntoIterator>::IntoIter,
    >;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        let xs = self.pa.parse(stream, logger)?.into_iter();
        let ys = self.pb.parse(stream, logger)?.into_iter();
        Some(xs.chain(ys))
    }
}

/// Kleene Closure Combinator
#[derive(Copy, Clone)]
pub struct Many<P> {
    parser: P,
}

impl<P> Many<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S: Clone, P: Parser<S>> Parser<S> for Many<P> {
    type Target = Vec<P::Target>;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        let mut vec = vec![];
        let mut s0 = stream.clone();
        let mut l0 = logger.clone();
        while let Some(a) = self.parser.parse(stream, logger) {
            vec.push(a);
            s0 = stream.clone();
            l0 = logger.clone();
        }
        *stream = s0;
        *logger = l0;
        Some(vec)
    }
}

/// Kleene Closure Combinator
#[derive(Copy, Clone)]
pub struct Many_<P> {
    parser: P,
}

impl<P> Many_<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S: Clone, P: Parser<S>> Parser<S> for Many_<P> {
    type Target = ();

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        let mut s0 = stream.clone();
        let mut l0 = logger.clone();
        while let Some(_) = self.parser.parse(stream, logger) {
            s0 = stream.clone();
            l0 = logger.clone();
        }
        *stream = s0;
        *logger = l0;
        Some(())
    }
}

/// Some Combinator
#[derive(Copy, Clone)]
pub struct Some<P> {
    parser: P,
}

impl<P> Some<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S: Clone, P: Parser<S>> Parser<S> for Some<P> {
    type Target = Vec<P::Target>;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        let mut vec = vec![];

        vec.push(self.parser.parse(stream, logger)?);

        let mut s0 = stream.clone();
        let mut l0 = logger.clone();
        while let Some(a) = self.parser.parse(stream, logger) {
            vec.push(a);
            s0 = stream.clone();
            l0 = logger.clone();
        }
        *stream = s0;
        *logger = l0;
        Some(vec)
    }
}

/// Some Combinator
#[derive(Copy, Clone)]
pub struct Some_<P> {
    parser: P,
}

impl<P> Some_<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S: Clone, P: Parser<S>> Parser<S> for Some_<P> {
    type Target = ();

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        self.parser.parse(stream, logger)?;
        let mut s0 = stream.clone();
        let mut l0 = logger.clone();
        while let Some(_) = self.parser.parse(stream, logger) {
            s0 = stream.clone();
            l0 = logger.clone();
        }
        *stream = s0;
        *logger = l0;
        Some(())
    }
}

/// Optional Combinator
#[derive(Copy, Clone)]
pub struct Optional<P> {
    parser: P,
}

impl<P> Optional<P> {
    pub fn new(parser: P) -> Self {
        Self { parser }
    }
}

impl<S: Clone, P: Parser<S>> Parser<S> for Optional<P> {
    type Target = Option<P::Target>;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        let s0 = stream.clone();
        let l0 = logger.clone();
        let fst = self.parser.parse(stream, logger);
        match fst {
            None => {
                *stream = s0;
                *logger = l0;
                Some(None)
            }
            ok => ok.map(Some),
        }
    }
}

/// Flatten Combinator
#[derive(Copy, Clone, Debug)]
pub struct Flatten<P> {
    pp: P,
}

impl<P> Flatten<P> {
    pub fn new(pp: P) -> Self {
        Self { pp }
    }
}

impl<S, P> Parser<S> for Flatten<P>
where
    P: Parser<S>,
    P::Target: IntoParser<S>,
{
    type Target = <<P as Parser<S>>::Target as IntoParser<S>>::Target;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        self.pp
            .parse(stream, logger)?
            .into_parser()
            .parse(stream, logger)
    }
}

/// Multiple Choice Combinator
pub struct Choice<S, A> {
    ps: Vec<Box<dyn Parser<S, Target = A>>>,
}

impl<S, A> Choice<S, A> {
    pub fn new(ps: Vec<Box<dyn Parser<S, Target = A>>>) -> Self {
        Self { ps }
    }
}

pub fn choice<S: Clone, A>(ps: Vec<Box<dyn Parser<S, Target = A>>>) -> Choice<S, A> {
    Choice::new(ps)
}

impl<S: Clone, A> Parser<S> for Choice<S, A> {
    type Target = A;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        for p in self.ps.iter() {
            let tmp = stream.clone();
            if let ok @ Some(_) = p.parse(stream, logger) {
                return ok;
            }
            *stream = tmp;
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct Logger<P> {
    log: Msg,
    parser: P,
}

impl<P> Logger<P> {
    pub fn new(parser: P, log: Msg) -> Self {
        Self { log, parser }
    }
}

impl<S, P: Parser<S>> Parser<S> for Logger<P> {
    type Target = P::Target;

    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        match self.parser.parse(stream, logger) {
            None => {
                logger.add(self.log.clone());
                None
            }
            ok => ok,
        }
    }
}
