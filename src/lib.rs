#![feature(try_trait)]
pub mod core;

pub use crate::core::{
    combinator::{
        adaptor::{self, empty, join, pure, select},
        basic::{self, char, eof, reg, satisfy, strg},
        compose::{self, blank, digit, letter, lexeme},
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
