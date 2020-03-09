pub trait ParserLogger {
    fn info(&mut self, msg: &str);
    fn warn(&mut self, msg: &str);
    fn err(&mut self, msg: &str);
    fn clear(&mut self);
}

#[derive(Debug, Clone)]
pub enum Msg {
    Info(String),
    Warn(String),
    Err(String),
}

#[derive(Debug, Default, Clone)]
pub struct ParseErr {
    stack: Vec<Msg>,
}

impl ParserLogger for ParseErr {
    fn info(&mut self, msg: &str) {
        self.stack.push(Msg::Info(msg.to_string()));
    }

    fn warn(&mut self, msg: &str) {
        self.stack.push(Msg::Warn(msg.to_string()));
    }

    fn err(&mut self, msg: &str) {
        self.stack.push(Msg::Err(msg.to_string()));
    }

    fn clear(&mut self) {
        self.stack.clear();
    }
}
