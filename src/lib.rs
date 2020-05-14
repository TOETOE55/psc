pub mod core;

pub use crate::core::{
    combinator::{
        self,
        adaptor::{self, choice, empty, fix, pure},
        basic::{self, char, reg, satisfy, strg, eof, pos},
        combine::{self, alpha, blank, digit, lexeme},
        ops::{self, wrap, ParseFn},
    },
    traits::{
        self, covert,
        err::{Msg, ParseLogger},
        ext::ParserExt,
        parser::Parser,
        stream::{ParseState, Stream, Pos},
    },
};

#[cfg(test)]
mod tests;
