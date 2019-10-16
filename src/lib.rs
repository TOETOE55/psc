pub mod core;

pub use crate::core::{
    combinators::{
        common::{char, reg, satisfy, strg, Char, Regex, Satisfy, Strg},
        eof, failure, fix,
        ops::{ParseFn, ParserWrapper},
        pure, Failure, Fix, EOF,
    },
    err::ParseMsg,
    state::{ParseState, Pos},
    traits::{covert, parser::Parser, stream::Stream},
};

#[cfg(test)]
mod tests;
