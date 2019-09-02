
pub trait Stream {
    type Item;
    fn next(&self) -> Option<(Self::Item, Self)> where Self: Sized;

}

impl Stream for &str {
    type Item = char;
    fn next(&self) -> Option<(Self::Item, Self)> {
        let mut chars = self.chars();
        match chars.next() {
            Some(c) => Some((c, chars.as_str())),
            _             => None,
        }
    }
}