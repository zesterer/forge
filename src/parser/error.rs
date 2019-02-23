use std::fmt;
use super::{
    Item,
    SrcRef,
};
use crate::output;

#[derive(Debug)]
pub enum ParseError {
    Phoney,
    UnexpectedChar(char),
    ExpectedDelimiter(char),
    Expected(Item, Item), // Expected, found
    ReservedKeyword(String),
    At(SrcRef, Box<ParseError>),
    Many(Vec<ParseError>),
}

impl ParseError {
    pub fn phoney() -> Self {
        ParseError::Phoney
    }

    pub fn max(self, other: Self) -> Self {
        match (self, other) {
            (ParseError::At(r0, e0), ParseError::At(r1, e1)) => if r0.limit() > r1.limit() {
                ParseError::At(r0, e0)
            } else {
                ParseError::At(r1, e1)
            },
            (ParseError::Phoney, other) => other,
            (this, ParseError::Phoney) => this,
            (this, _) => this,
        }
    }

    pub fn fmt_nice_located(&self, f: &mut fmt::Formatter, src: Option<&str>, depth: usize, r: SrcRef) -> fmt::Result {
        match self {
            ParseError::UnexpectedChar(c) => {
                Ok(())
                    .and_then(|_| writeln!(f, "Parse error at {}...", r.start()))
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Unexpected character '{}' in code.", output::Repeat(' ', (depth + 1) * 3), c))
            },
            ParseError::ExpectedDelimiter(c) => {
                Ok(())
                    .and_then(|_| writeln!(f, "Parse error at {}...", r.start()))
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Unexpected token delimiter. Are you missing a '{}'?", output::Repeat(' ', (depth + 1) * 3), c))
            },
            ParseError::Expected(expected, found) => {
                Ok(())
                    .and_then(|_| writeln!(f, "Parse error at {}...", r.start()))
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Expected {}, found {}.", output::Repeat(' ', (depth + 1) * 3), expected, found))
            },
            ParseError::ReservedKeyword(keyword) => {
                Ok(())
                    .and_then(|_| writeln!(f, "Parse error at {}...", r.start()))
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Use of keyword '{}' is not permitted because it is reserved for future use.", output::Repeat(' ', (depth + 1) * 3), keyword))
            },
            _ => Ok(()),
        }
    }

    pub fn fmt_nice(&self, f: &mut fmt::Formatter, src: Option<&str>, depth: usize) -> fmt::Result {
        match self {
            ParseError::At(r, err) => err.fmt_nice_located(f, src, depth, *r),
            ParseError::Many(errs) => errs.iter().try_for_each(|err| err.fmt_nice(f, src, depth)),
            _ => Ok(()),
        }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;
