#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ParseMsg {
    Except(String),
    UnExcept(String),
    Warn(String),
    Info(String),
    EOF,
}
