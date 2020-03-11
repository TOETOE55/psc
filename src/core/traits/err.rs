#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Msg {
    Info(String),
    Warn(String),
    Err(String),
}

#[derive(Clone, Eq, PartialEq, Debug, Default)]
pub struct ParseLogger {
    stack: Vec<Msg>,
}

impl ParseLogger {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, msg: Msg) {
        self.stack.push(msg);
    }

    pub fn with(&mut self, msg: Msg) {
        self.stack.clear();
        self.stack.push(msg);
    }
}

impl IntoIterator for ParseLogger {
    type Item = Msg;
    type IntoIter = std::vec::IntoIter<Msg>;

    fn into_iter(self) -> Self::IntoIter {
        self.stack.into_iter()
    }
}

impl<'a> IntoIterator for &'a ParseLogger {
    type Item = &'a Msg;
    type IntoIter = std::slice::Iter<'a, Msg>;

    fn into_iter(self) -> Self::IntoIter {
        self.stack.iter()
    }
}

impl<'a> IntoIterator for &'a mut ParseLogger {
    type Item = &'a mut Msg;
    type IntoIter = std::slice::IterMut<'a, Msg>;

    fn into_iter(self) -> Self::IntoIter {
        self.stack.iter_mut()
    }
}
