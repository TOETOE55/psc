use crate::ParseLogger;
use std::rc::Rc;

/// An interface for dealing with Parser Combinator.
/// This is the main parser trait.
pub trait Parser<S> {
    /// The type of the target after parsed.
    type Target;

    /// Parse the stream and returning the result with the rest of stream.
    /// Returns Err(ParseMsg) when the parsing failed or at the end of the stream.
    ///
    /// # Example
    /// Basic usage:
    /// ```
    /// use psc::{satisfy, char, Parser, ParserExt, ParseState, ParseLogger};
    ///
    /// let parser = char('+').or(char('-')).option();
    /// // ('+'|'-')?
    ///
    /// let mut src = ParseState::new("+123");
    /// let mut logger = ParseLogger::default();
    /// let res = parser.parse(&mut src, &mut logger).unwrap();
    /// assert_eq!(res, Some('+'));
    /// assert_eq!(src.as_str(), "123");
    ///
    /// ```
    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target>;
}

impl<S, P: Parser<S> + ?Sized> Parser<S> for &P {
    type Target = P::Target;
    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        (**self).parse(stream, logger)
    }
}

impl<S, P: Parser<S> + ?Sized> Parser<S> for &mut P {
    type Target = P::Target;
    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        (**self).parse(stream, logger)
    }
}

impl<S, P: Parser<S> + ?Sized> Parser<S> for Box<P> {
    type Target = P::Target;
    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        (**self).parse(stream, logger)
    }
}

impl<S, P: Parser<S> + ?Sized> Parser<S> for Rc<P> {
    type Target = P::Target;
    fn parse(&self, stream: &mut S, logger: &mut ParseLogger) -> Option<Self::Target> {
        (**self).parse(stream, logger)
    }
}
