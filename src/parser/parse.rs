use std::{
    slice,
    rc::Rc,
};
use super::{
    ParseError,
    ParseResult,
    SrcRef,
    SrcLoc,
    Token,
    Lexeme,
    ast::{
        Expr,
        Node,
    },
};

#[derive(Debug)]
pub enum Item {
    Lexeme(Lexeme),
    Ident,
    Primary,
    End,
}

fn expected(expected: Item, found: Item, src_ref: SrcRef) -> ParseError {
    ParseError::At(
        src_ref,
        Box::new(ParseError::Expected(expected, found)),
    )
}

#[derive(Clone)]
pub struct ParseCtx<'a> {
    tokens: slice::Iter<'a, Token>,
}

impl<'a> ParseCtx<'a> {
    pub fn new(tokens: slice::Iter<'a, Token>) -> Self {
        Self {
            tokens,
        }
    }

    pub fn src_ref(&self) -> SrcRef {
        self.tokens
            .clone()
            .next()
            .map(|t| t.1.clone())
            .unwrap_or(SrcRef::end())
    }

    pub fn advance(&mut self) {
        self.tokens.next();
    }

    pub fn peek(&self) -> Token {
        self.tokens.clone().next().unwrap_or(&Token(Lexeme::Eof, SrcRef::end())).clone()
    }

    pub fn read_lexeme(&mut self, lexeme: Lexeme) -> ParseResult<SrcRef> {
        match self.peek() {
            Token(l, r) if l == lexeme => {
                self.advance();
                Ok(r)
            },
            Token(l, r) => Err(expected(Item::Lexeme(lexeme), Item::Lexeme(l), r)),
        }
    }

    pub fn read_ident(&mut self) -> ParseResult<Node<String>> {
        match self.peek() {
            Token(Lexeme::Ident(s), r) => {
                self.advance();
                Ok(Node(s, r))
            },
            Token(l, r) => Err(expected(Item::Ident, Item::Lexeme(l), r)),
        }
    }

    pub fn read_primary(&mut self) -> ParseResult<Node<Expr>> {
        let expr = match self.peek() {
            Token(Lexeme::Number(x), r) => Node(Expr::LiteralNumber(x), r),
            Token(Lexeme::String(s), r) => Node(Expr::LiteralString(s), r),
            Token(Lexeme::True, r) => Node(Expr::LiteralBoolean(true), r),
            Token(Lexeme::False, r) => Node(Expr::LiteralBoolean(false), r),
            Token(Lexeme::Null, r) => Node(Expr::LiteralNull, r),
            Token(Lexeme::Ident(s), r) => Node(Expr::Ident(s), r),
            Token(l, r) => return Err(expected(Item::Primary, Item::Lexeme(l), r)),
        };
        self.advance();
        Ok(expr)
    }

    pub fn read_access(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        let mut expr = self.read_primary()?;

        loop {
            let mut this = self.clone();
            expr = match this.read_member() {
                Ok((dot_r, Node(ident, r))) => {
                    *self = this;
                    let r_union = expr.1.union(&r).union(&dot_r);
                    Node(Expr::DotAccess(dot_r, Box::new(expr), Node(ident, r)), r_union)
                },
                Err(err) => return Ok((expr, err)),
            }
        }
    }

    pub fn read_call(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        let (mut expr, max_err) = self.read_access()?;

        loop {
            let mut this = self.clone();
            expr = match this.read_paramlist() {
                Ok(Node(params, params_r)) => {
                    *self = this;
                    let r_union = params
                        .iter()
                        .fold(SrcRef::empty(), |r, p| p.1.union(&r));
                    Node(Expr::Call(params_r, Box::new(expr), params), r_union)
                },
                Err(err) => return Ok((expr, err.max(max_err))),
            };
        }
    }

    pub fn read_unary(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        Ok(match self.peek() {
            Token(Lexeme::Bang, r) => {
                self.advance();
                let (operand, err) = self.read_unary()?;
                let r_union = r.union(&operand.1);
                (Node(Expr::UnaryNot(r, Box::new(operand)), r_union), err)
            },
            Token(Lexeme::Minus, r) => {
                self.advance();
                let (operand, err) = self.read_unary()?;
                let r_union = r.union(&operand.1);
                (Node(Expr::UnaryNeg(r, Box::new(operand)), r_union), err)
            },
            _ => self.read_call()?,
        })
    }

    pub fn read_multiplication(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        let (mut expr, mut max_err) = self.read_unary()?;

        loop {
            match self.peek() {
                Token(Lexeme::Star, r) => {
                    self.advance();
                    let (operand, err) = self.read_unary()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryMul(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(Lexeme::Slash, r) => {
                    self.advance();
                    let (operand, err) = self.read_unary()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryDiv(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(l, r) => return Ok((expr, max_err)),
            };
        }
    }

    pub fn read_addition(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        let (mut expr, mut max_err) = self.read_multiplication()?;

        loop {
            match self.peek() {
                Token(Lexeme::Plus, r) => {
                    self.advance();
                    let (operand, err) = self.read_multiplication()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryAdd(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(Lexeme::Minus, r) => {
                    self.advance();
                    let (operand, err) = self.read_multiplication()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinarySub(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(l, r) => return Ok((expr, max_err)),
            };
        }
    }

    pub fn read_comparison(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        let (mut expr, mut max_err) = self.read_addition()?;

        loop {
            match self.peek() {
                Token(Lexeme::Greater, r) => {
                    self.advance();
                    let (operand, err) = self.read_addition()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryGreater(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(Lexeme::GreaterEq, r) => {
                    self.advance();
                    let (operand, err) = self.read_addition()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryGreaterEq(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(Lexeme::Less, r) => {
                    self.advance();
                    let (operand, err) = self.read_addition()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryLess(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(Lexeme::LessEq, r) => {
                    self.advance();
                    let (operand, err) = self.read_addition()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryLessEq(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(l, r) => return Ok((expr, max_err)),
            };
        }
    }

    pub fn read_equivalence(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        let (mut expr, mut max_err) = self.read_comparison()?;

        loop {
            match self.peek() {
                Token(Lexeme::Eq, r) => {
                    self.advance();
                    let (operand, err) = self.read_comparison()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryEq(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(Lexeme::BangEq, r) => {
                    self.advance();
                    let (operand, err) = self.read_comparison()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryNotEq(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(l, r) => return Ok((expr, max_err)),
            };
        }
    }

    pub fn read_expr(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        self.read_equivalence()
    }

    pub fn read_expr_full(&mut self) -> ParseResult<Expr> {
        let (expr, max_err) = self.read_expr()?;
        match self.peek() {
            Token(Lexeme::Eof, _) => Ok(expr.0),
            Token(l, r) => Err(expected(Item::End, Item::Lexeme(l), r).max(max_err)),
        }
    }

    pub fn read_member(&mut self) -> ParseResult<(SrcRef, Node<String>)> {
        let dot_r = match self.peek() {
            Token(Lexeme::Dot, r) => { self.advance(); r},
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::Dot), Item::Lexeme(l), r))
        };
        Ok((dot_r, self.read_ident()?))
    }

    pub fn read_paramlist(&mut self) -> ParseResult<Node<Vec<Node<Expr>>>> {
        Err(ParseError::UnexpectedChar('!'))
    }
}
