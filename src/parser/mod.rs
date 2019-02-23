pub mod ast;
pub mod lexer;
pub mod parse;
pub mod src;

// Reexports
pub use self::src::{
    SrcRef,
    SrcLoc,
};

use std::rc::Rc;
use self::{
    lexer::{
        lex,
        Lexeme,
        Token,
    },
    parse::{
        Item,
        ParseCtx,
    },
    ast::Expr,
};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedChar(char),
    UnexpectedEof,
    Expected(Item, Item), // Expected, found
    ReservedKeyword(String),
    At(SrcRef, Box<ParseError>),
    Many(Vec<ParseError>),
}

impl ParseError {
    pub fn max(self, other: Self) -> Self {
        match (self, other) {
            (ParseError::At(r0, e0), ParseError::At(r1, e1)) => if r0.limit() > r1.limit() {
                ParseError::At(r0, e0)
            } else {
                ParseError::At(r1, e1)
            },
            (this, _) => this,
        }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser {
    tokens: Vec<Token>,
    code: String,
}

impl Parser {
    pub fn new(code: &str) -> ParseResult<Self> {
        Ok(Self {
            tokens: lex(code)?,
            code: code.to_string(),
        })
    }

    pub fn parse_expr(&self) -> ParseResult<Expr> {
        // TODO: Remove this
        /*
        for tok in &self.tokens {
            println!("{:?}", tok);
        }
        */

        ParseCtx::new(self.tokens.iter()).read_expr_full()
    }
}
