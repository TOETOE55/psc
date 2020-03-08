use std::fmt::Debug;

pub trait ParseError {
    fn info(&mut self, msg: &str);
    fn warn(&mut self, msg: &str);
    fn err(&mut self, msg: &str);
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

impl ParseError for ParseErr {
    fn info(&mut self, msg: &str) {
        self.stack.push(Msg::Info(msg.to_string()));
    }

    fn warn(&mut self, msg: &str) {
        self.stack.push(Msg::Warn(msg.to_string()));
    }

    fn err(&mut self, msg: &str) {
        self.stack.push(Msg::Err(msg.to_string()));
    }
}