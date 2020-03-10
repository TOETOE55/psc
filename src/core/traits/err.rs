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
