pub mod ast;
pub mod error;
pub mod lexer;
pub mod parse;
pub mod src;

// Reexports
pub use self::{
    src::{
        SrcRef,
        SrcLoc,
    },
    error::{
        ParseError,
        ParseResult,
    },
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
    ast::{
        Node,
        Expr,
        Stmt,
    },
};

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

    pub fn parse_stmts(&self) -> ParseResult<Vec<Node<Stmt>>> {
        ParseCtx::new(self.tokens.iter()).read_stmts_full()
    }
}
