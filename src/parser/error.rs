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
    WhileParsing(String, Box<ParseError>),
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
            (ParseError::WhileParsing(elem, this), other) => if this.further_than(&other) { ParseError::WhileParsing(elem, this) } else { other },
            (this, ParseError::WhileParsing(elem, other)) => if this.further_than(&other) { this } else { ParseError::WhileParsing(elem, other) },
            (ParseError::Phoney, other) => other,
            (this, ParseError::Phoney) => this,
            (this, _) => this,
        }
    }

    pub fn further_than(&self, other: &Self) -> bool {
        match (self, other) {
            (ParseError::At(r0, _), ParseError::At(r1, _)) => if r0.limit() > r1.limit() {
                true
            } else {
                false
            },
            (ParseError::WhileParsing(_, this), other) => this.further_than(other),
            (this, ParseError::WhileParsing(_, other)) => this.further_than(other),
            (ParseError::Phoney, _) => false,
            (_, ParseError::Phoney) => true,
            (_, _) => true,
        }
    }

    pub fn while_parsing(self, element: &str) -> Self {
        ParseError::WhileParsing(element.to_string(), Box::new(self))
    }

    pub fn fmt_nice_located(&self, f: &mut fmt::Formatter, src: Option<&str>, depth: usize, r: SrcRef, while_parsing: Vec<String>) -> fmt::Result {
        let indent = output::Repeat(' ', (depth + 1) * 3);
        writeln!(f, "[ERROR] Parsing error at {}...", r.start())?;
        for elem in &while_parsing {
            writeln!(f, "{}...while parsing {}...", indent, elem)?;
        }
        match self {
            ParseError::UnexpectedChar(c) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Unexpected character '{}' in code.", indent, c))
            },
            ParseError::ExpectedDelimiter(c) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Expected token delimiter. Are you missing a '{}'?", indent, c))
            },
            ParseError::Expected(expected, found) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Expected {}, found {}.", indent, expected, found))
            },
            ParseError::ReservedKeyword(keyword) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Use of keyword '{}' is not permitted because it is reserved for future use.", indent, keyword))
            },
            _ => Ok(()),
        }
    }

    pub fn fmt_nice(&self, f: &mut fmt::Formatter, src: Option<&str>, depth: usize, mut while_parsing: Vec<String>) -> fmt::Result {
        match self {
            ParseError::WhileParsing(element, err) => {
                while_parsing.push(element.clone());
                Ok(())
                    .and_then(|_| err.fmt_nice(f, src, depth, while_parsing))
            },
            ParseError::At(r, err) => err.fmt_nice_located(f, src, depth, *r, while_parsing),
            ParseError::Many(errs) => errs.iter().try_for_each(|err| err.fmt_nice(f, src, depth, while_parsing.clone())),
            _ => Ok(()),
        }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;
