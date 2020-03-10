pub mod core;

pub use crate::core::{
    combinator::{
        self,
        adaptor::{self, empty, pure, choice, fix},
        basic::{self, satisfy, strg, char, reg, EOF},
        ops::{self, wrap, ParseFn},
        combine::{self, blank, alpha, digit, lexeme}
    },
    traits::{
        self, covert,
        err::{Msg, ParseLogger},
        ext::ParserExt,
        parser::Parser,
        stream::{
            Stream,
            ParseState,
        },
    },
};

#[cfg(test)]
mod tests;
