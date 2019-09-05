
/// The trait of parse stream.
pub trait Stream: Sized {
    /// The type of stream elements.
    type Item;
    /// Consume a stream and returning the next element of stream and the rest of stream.
    fn next(self) -> Option<(Self::Item, Self)>;
    /// Return the empty stream.
    fn empty() -> Self;
}

impl Stream for &str {
    type Item = char;
    fn next(self) -> Option<(Self::Item, Self)> {
        let mut chars = self.chars();
        match chars.next() {
            Some(c) => Some((c, chars.as_str())),
            _             => None,
        }
    }

    fn empty() -> Self {
        ""
    }
}