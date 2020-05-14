pub mod core;

pub use crate::core::{
    combinator::{
        self,
        adaptor::{self, choice, empty, fix, pure},
        basic::{self, char, reg, satisfy, strg, EOF},
        combine::{self, alpha, blank, digit, lexeme},
        ops::{self, wrap, ParseFn},
    },
    traits::{
        self, covert,
        err::{Msg, ParseLogger},
        ext::ParserExt,
        parser::Parser,
        stream::{ParseState, Stream},
    },
};

#[cfg(test)]
mod tests;
