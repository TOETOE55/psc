#![feature(try_trait)]
pub mod core;

pub use crate::core::{
    combinator::{
        adaptor::{self, empty, join, pure, select},
        basic::{self, char, digit, eof, letter, satisfy, strg, regex},
        ops::{self, wrap, ParseFn, ParserWrapper},
    },
    traits::{
        covert,
        err::{self, ParseErr},
        extend::{self, ParserExt},
        parser::{self, Consumed, ParseResult, Parser},
        stream::{self, ParseState, Pos, Stream},
    },
};

#[cfg(test)]
mod tests;
