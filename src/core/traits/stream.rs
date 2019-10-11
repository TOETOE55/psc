use std::str::Chars;

/// The trait of parse stream.
pub trait Stream: Iterator {}

impl<'a> Stream for Chars<'a> {}
