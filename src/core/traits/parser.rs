use crate::core::traits::err::ParseErr;

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
    use psc::*;

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
