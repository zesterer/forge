use crate::{
    parser::ParseError,
    exec::ExecError,
};

#[derive(Debug)]
pub enum ForgeError {
    Parse(ParseError),
    Exec(ExecError),
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
