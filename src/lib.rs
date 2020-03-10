pub mod core;

pub use crate::core::{
    combinator::{
        self,
        adaptor::{self, empty, pure, choice},
        basic::{self, satisfy, strg, char, reg, EOF},
        ops::{self, wrap},
        combine::{self, blank, alpha, digit, lexeme}
    },
    traits::{
        self, covert,
        err::{Msg, ParseLogger},
        ext::ParserExt,
        parser::Parser,
        stream::Stream,
    },
};

#[cfg(test)]
mod tests;
