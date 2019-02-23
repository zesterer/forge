use std::fmt;
use crate::{
    parser::ParseError,
    exec::ExecError,
};

#[derive(Debug)]
pub enum ForgeError {
    Parse(ParseError),
    Exec(ExecError),
    InSrc(String, Box<ForgeError>),
}

pub type ForgeResult<T> = Result<T, ForgeError>;

impl From<ParseError> for ForgeError {
    fn from(err: ParseError) -> Self {
        ForgeError::Parse(err)
    }
}

impl From<ExecError> for ForgeError {
    fn from(err: ExecError) -> Self {
        ForgeError::Exec(err)
    }
}

impl ForgeError {
    fn fmt_nice(&self, f: &mut fmt::Formatter, src: Option<&str>, _depth: usize) -> fmt::Result {
        match self {
            ForgeError::Parse(err) => err.fmt_nice(f, src, 0, vec![]),
            ForgeError::Exec(err) => err.fmt_nice(f, src, 0),
            _ => Ok(()),
        }
    }
}

impl fmt::Display for ForgeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ForgeError::InSrc(src, err) => err.fmt_nice(f, Some(src), 0),
            err => err.fmt_nice(f, None, 0),
        }
    }
}
