use std::{
    slice,
    fmt,
    rc::Rc,
};
use super::{
    ParseError,
    ParseResult,
    SrcRef,
    Token,
    Lexeme,
    ast::{
        Node,
        Expr,
        Stmt,
        Block,
        Args,
    },
};

#[derive(Debug, PartialEq)]
pub enum Item {
    Lexeme(Lexeme),
    Ident,
    Primary,
    Stmt,
    End,
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Item::Lexeme(lexeme) => match lexeme {
                Lexeme::Ident(ident) => write!(f, "identifier '{}'", ident),
                lexeme => write!(f, "'{}'", lexeme),
            },
            Item::Ident => write!(f, "identifier"),
            Item::Primary => write!(f, "primary expression"),
            Item::Stmt => write!(f, "statement"),
            Item::End => write!(f, "end of input"),
        }
    }
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
    code: Rc<String>,
}

impl<'a> ParseCtx<'a> {
    pub fn new(tokens: slice::Iter<'a, Token>, code: Rc<String>) -> Self {
        Self {
            tokens,
            code,
        }
    }

    #[allow(dead_code)]
    pub fn src_ref(&self) -> SrcRef {
        self.tokens
            .clone()
            .next()
            .map(|t| t.1.clone())
            .unwrap_or(SrcRef::end())
    }

    fn advance(&mut self) {
        self.tokens.next();
    }

    fn peek(&self) -> Token {
        self.tokens.clone().next().unwrap_or(&Token(Lexeme::Eof, SrcRef::end())).clone()
    }

    fn read_ident(&mut self) -> ParseResult<Node<String>> {
        match self.peek() {
            Token(Lexeme::Ident(s), r) => {
                self.advance();
                Ok(Node(s, r))
            },
            Token(l, r) => Err(expected(Item::Ident, Item::Lexeme(l), r)),
        }
    }

    fn read_primary(&mut self) -> ParseResult<(Node<Expr>, Option<ParseError>)> {
        let expr = match self.peek() {
            Token(Lexeme::Number(x), r) => Node(Expr::LiteralNumber(x), r),
            Token(Lexeme::String(s), r) => Node(Expr::LiteralString(s), r),
            Token(Lexeme::True, r) => Node(Expr::LiteralBoolean(true), r),
            Token(Lexeme::False, r) => Node(Expr::LiteralBoolean(false), r),
            Token(Lexeme::Null, r) => Node(Expr::LiteralNull, r),
            Token(Lexeme::Ident(s), r) => Node(Expr::Ident(Node(s, r)), r),
            Token(Lexeme::LParen, _r) => {
                let mut this = self.clone();
                let (paren_expr, err) = this.read_paren_expr()?;
                *self = this;
                return Ok((paren_expr, Some(err)));
            },
            Token(Lexeme::Pipe, _r) => {
                let mut this = self.clone();
                let (fn_expr, err) = this.read_fn_expr()?;
                *self = this;
                return Ok((fn_expr, Some(err)));
            },
            Token(l, r) => return Err(expected(Item::Primary, Item::Lexeme(l), r)),
        };
        self.advance();
        Ok((expr, None))
    }

    fn read_access(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        let (mut expr, err) = self.read_primary()?;

        let max_err = err.unwrap_or(ParseError::phoney());

        loop {
            let mut this = self.clone();
            expr = match this.read_member() {
                Ok((dot_r, Node(ident, r))) => {
                    *self = this;
                    let r_union = expr.1.union(&r).union(&dot_r);
                    Node(Expr::DotAccess(dot_r, Box::new(expr), Node(ident, r)), r_union)
                },
                Err(err) => return Ok((expr, err.max(max_err))),
            }
        }
    }

    fn read_call(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        let (mut expr, mut max_err) = self.read_access()?;

        loop {
            let mut this = self.clone();
            match this.read_params() {
                Ok((Node(params, params_r), err)) => {
                    *self = this;
                    let r_union = params
                        .iter()
                        .fold(SrcRef::empty(), |r, p| p.1.union(&r));
                    expr = Node(Expr::Call(params_r, Box::new(expr), Node(params, params_r)), r_union);
                    max_err = err.max(max_err);
                },
                Err(err) => return Ok((expr, err.max(max_err))),
            };
        }
    }

    fn read_unary(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
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

    fn read_multiplication(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
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
                Token(Lexeme::Percent, r) => {
                    self.advance();
                    let (operand, err) = self.read_unary()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryMod(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(_l, _r) => return Ok((expr, max_err)),
            };
        }
    }

    fn read_addition(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
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
                Token(_l, _r) => return Ok((expr, max_err)),
            };
        }
    }

    fn read_mid_unary(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        Ok(match self.peek() {
            Token(Lexeme::Input, r) => {
                self.advance();
                let (operand, err) = self.read_mid_unary()?;
                let r_union = r.union(&operand.1);
                (Node(Expr::UnaryInput(r, Box::new(operand)), r_union), err)
            },
            _ => self.read_addition()?,
        })
    }

    fn read_comparison(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        let (mut expr, mut max_err) = self.read_mid_unary()?;

        loop {
            match self.peek() {
                Token(Lexeme::Greater, r) => {
                    self.advance();
                    let (operand, err) = self.read_mid_unary()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryGreater(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(Lexeme::GreaterEq, r) => {
                    self.advance();
                    let (operand, err) = self.read_mid_unary()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryGreaterEq(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(Lexeme::Less, r) => {
                    self.advance();
                    let (operand, err) = self.read_mid_unary()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryLess(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(Lexeme::LessEq, r) => {
                    self.advance();
                    let (operand, err) = self.read_mid_unary()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryLessEq(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(_l, _r) => return Ok((expr, max_err)),
            };
        }
    }

    fn read_equivalence(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
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
                Token(_l, _r) => return Ok((expr, max_err)),
            };
        }
    }

    fn read_logical(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        let (mut expr, mut max_err) = self.read_equivalence()?;

        loop {
            match self.peek() {
                Token(Lexeme::And, r) => {
                    self.advance();
                    let (operand, err) = self.read_equivalence()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryAnd(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(Lexeme::Or, r) => {
                    self.advance();
                    let (operand, err) = self.read_equivalence()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryOr(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(Lexeme::Xor, r) => {
                    self.advance();
                    let (operand, err) = self.read_equivalence()?;
                    let r_union = r.union(&expr.1).union(&operand.1);
                    expr = Node(Expr::BinaryXor(r, Box::new(expr), Box::new(operand)), r_union);
                    max_err = err.max(max_err);
                },
                Token(_l, _r) => return Ok((expr, max_err)),
            };
        }
    }

    fn read_expr(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        self.read_logical().map_err(|err| err.while_parsing("expression"))
    }

    fn read_paren_expr(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        let r_start = match self.peek() {
            Token(Lexeme::LParen, r) => { self.advance(); r },
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::LParen), Item::Lexeme(l), r)),
        };

        let (expr, max_err) = self.read_expr()?;

        match self.peek() {
            Token(Lexeme::RParen, r) => {
                self.advance();
                let r_union = expr.1.union(&r_start).union(&r);
                Ok((Node(expr.0, r_union), max_err))
            },
            Token(l, r) => Err(expected(Item::Lexeme(Lexeme::RParen), Item::Lexeme(l), r).max(max_err)),
        }
    }

    fn read_member(&mut self) -> ParseResult<(SrcRef, Node<String>)> {
        let dot_r = match self.peek() {
            Token(Lexeme::Dot, r) => { self.advance(); r},
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::Dot), Item::Lexeme(l), r))
        };
        Ok((dot_r, self.read_ident()?))
    }

    fn read_fn_expr(&mut self) -> ParseResult<(Node<Expr>, ParseError)> {
        const ELEMENT: &'static str = "function";

        let r_start = match self.peek() {
            Token(Lexeme::Pipe, r) => { self.advance(); r },
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::Pipe), Item::Lexeme(l), r).while_parsing(ELEMENT)),
        };

        let (args, max_err) = self.read_args().map_err(|err| err.while_parsing(ELEMENT))?;

        let r_middle = match self.peek() {
            Token(Lexeme::Pipe, r) => { self.advance(); r },
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::Pipe), Item::Lexeme(l), r).max(max_err).while_parsing(ELEMENT)),
        };

        let (block, max_err) = self.read_block().map_err(|err| err.max(max_err).while_parsing(ELEMENT))?;

        let r_union = args.1.union(&r_start).union(&r_middle).union(&block.1);
        Ok((Node(Expr::Fn(self.code.clone(), Rc::new((Node(args.0, args.1.union(&r_start).union(&r_middle)), block))), r_union), max_err.while_parsing(ELEMENT)))
    }

    fn read_paramlist(&mut self) -> ParseResult<(Node<Vec<Node<Expr>>>, ParseError)> {
        let mut params = vec![];
        let mut r_total = SrcRef::empty();
        let mut max_err = ParseError::Phoney;

        loop {
            let mut this = self.clone();
            match this.read_expr() {
                Ok((expr, err)) => {
                    *self = this;
                    r_total = r_total.union(&expr.1);
                    params.push(expr);
                    max_err = err.max(max_err);
                },
                Err(err) => {
                    max_err = err.max(max_err);
                    break;
                },
            }

            match self.peek() {
                Token(Lexeme::Comma, r) => {
                    self.advance();
                    r_total = r_total.union(&r);
                },
                Token(l, r) => {
                    max_err = expected(Item::Lexeme(Lexeme::Comma), Item::Lexeme(l), r).max(max_err);
                    break;
                },
            }
        }

        Ok((Node(params, r_total), max_err))
    }

    fn read_params(&mut self) -> ParseResult<(Node<Vec<Node<Expr>>>, ParseError)> {
        let r_start = match self.peek() {
            Token(Lexeme::LParen, r) => { self.advance(); r },
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::LParen), Item::Lexeme(l), r)),
        };

        let (params, max_err) = self.read_paramlist()?;

        match self.peek() {
            Token(Lexeme::RParen, r) => {
                self.advance();
                let r_union = params.1.union(&r_start).union(&r);
                Ok((Node(params.0, r_union), max_err))
            },
            Token(l, r) => Err(expected(Item::Lexeme(Lexeme::RParen), Item::Lexeme(l), r).max(max_err)),
        }
    }

    fn read_expr_stmt(&mut self) -> ParseResult<(Node<Stmt>, ParseError)> {
        const ELEMENT: &'static str = "expression statement";

        let (expr, max_err) = self.read_expr().map_err(|err| err.while_parsing(ELEMENT))?;

        match self.peek() {
            Token(Lexeme::Semicolon, r) => {
                self.advance();
                let r_union = expr.1.union(&r);
                Ok((Node(Stmt::Expr(expr), r_union), max_err))
            },
            Token(l, r) => Err(expected(Item::Lexeme(Lexeme::Semicolon), Item::Lexeme(l), r).max(max_err).while_parsing(ELEMENT)),
        }
    }

    fn read_print_stmt(&mut self) -> ParseResult<(Node<Stmt>, ParseError)> {
        const ELEMENT: &'static str = "print statement";

        let r_start = match self.peek() {
            Token(Lexeme::Print, r) => { self.advance(); r },
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::Print), Item::Lexeme(l), r).while_parsing(ELEMENT)),
        };

        let (expr, max_err) = self.read_expr().map_err(|err| err.while_parsing(ELEMENT))?;

        match self.peek() {
            Token(Lexeme::Semicolon, r) => {
                self.advance();
                let r_union = expr.1.union(&r_start).union(&r);
                Ok((Node(Stmt::Print(expr), r_union), max_err))
            },
            Token(l, r) => Err(expected(Item::Lexeme(Lexeme::Semicolon), Item::Lexeme(l), r).max(max_err).while_parsing(ELEMENT)),
        }
    }

    fn read_return_stmt(&mut self) -> ParseResult<(Node<Stmt>, ParseError)> {
        const ELEMENT: &'static str = "return statement";

        let r_start = match self.peek() {
            Token(Lexeme::Return, r) => { self.advance(); r },
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::Return), Item::Lexeme(l), r).while_parsing(ELEMENT)),
        };

        let (expr, max_err) = self.read_expr().map_err(|err| err.while_parsing(ELEMENT))?;

        match self.peek() {
            Token(Lexeme::Semicolon, r) => {
                self.advance();
                let r_union = expr.1.union(&r_start).union(&r);
                Ok((Node(Stmt::Return(expr), r_union), max_err))
            },
            Token(l, r) => Err(expected(Item::Lexeme(Lexeme::Semicolon), Item::Lexeme(l), r).max(max_err).while_parsing(ELEMENT)),
        }
    }

    fn read_if_else_stmt(&mut self) -> ParseResult<(Node<Stmt>, ParseError)> {
        const ELEMENT: &'static str = "if-else statement";

        let r_start = match self.peek() {
            Token(Lexeme::If, r) => { self.advance(); r },
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::If), Item::Lexeme(l), r).while_parsing(ELEMENT)),
        };

        let (expr, max_err) = self.read_expr().map_err(|err| err.while_parsing(ELEMENT))?;

        let (true_block, max_err) = match self.read_block() {
            Ok((block, err)) => {
                (block, err.max(max_err))
            }
            Err(err) => return Err(err.max(max_err).while_parsing(ELEMENT)),
        };

        let r_else = match self.peek() {
            Token(Lexeme::Else, r) => { self.advance(); r },
            Token(l, r) => {
                let r_union = expr.1.union(&r_start);
                return Ok((Node(Stmt::If(expr, true_block), r_union), expected(Item::Lexeme(Lexeme::Else), Item::Lexeme(l), r).max(max_err).while_parsing(ELEMENT)))
            },
        };

        match self.read_block() {
            Ok((block, err)) => {
                let r_union = expr.1.union(&r_start).union(&r_else).union(&block.1);
                Ok((Node(Stmt::IfElse(expr, true_block, block), r_union), err.max(max_err).while_parsing(ELEMENT)))
            }
            Err(err) => Err(err.max(max_err).while_parsing(ELEMENT)),
        }
    }

    fn read_while_stmt(&mut self) -> ParseResult<(Node<Stmt>, ParseError)> {
        const ELEMENT: &'static str = "while statement";

        let r_start = match self.peek() {
            Token(Lexeme::While, r) => { self.advance(); r },
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::While), Item::Lexeme(l), r).while_parsing(ELEMENT)),
        };

        let (expr, max_err) = self.read_expr().map_err(|err| err.while_parsing(ELEMENT))?;

        match self.read_block() {
            Ok((block, err)) => {
                let r_union = expr.1.union(&r_start).union(&block.1);
                Ok((Node(Stmt::While(expr, block), r_union), err.max(max_err).while_parsing(ELEMENT)))
            }
            Err(err) => Err(err.max(max_err).while_parsing(ELEMENT)),
        }
    }

    fn read_decl_stmt(&mut self) -> ParseResult<(Node<Stmt>, ParseError)> {
        const ELEMENT: &'static str = "variable declaration";

        let r_start = match self.peek() {
            Token(Lexeme::Var, r) => { self.advance(); r },
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::Var), Item::Lexeme(l), r).while_parsing(ELEMENT)),
        };

        let (ident, r_ident) = match self.peek() {
            Token(Lexeme::Ident(s), r) => { self.advance(); (s.clone(), r) },
            Token(l, r) => return Err(expected(Item::Ident, Item::Lexeme(l), r).while_parsing(ELEMENT)),
        };

        let r_assign = match self.peek() {
            Token(Lexeme::Assign, r) => { self.advance(); r },
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::Assign), Item::Lexeme(l), r).while_parsing(ELEMENT)),
        };

        let (expr, max_err) = self.read_expr().map_err(|err| err.while_parsing(ELEMENT))?;

        match self.peek() {
            Token(Lexeme::Semicolon, r) => {
                self.advance();
                let r_union = expr.1.union(&r_start).union(&r_ident).union(&r_assign).union(&r);
                Ok((Node(Stmt::Decl(Node(ident, r_ident), expr), r_union), max_err))
            },
            Token(l, r) => Err(expected(Item::Lexeme(Lexeme::Semicolon), Item::Lexeme(l), r).max(max_err).while_parsing(ELEMENT)),
        }
    }

    fn read_assign_stmt(&mut self) -> ParseResult<(Node<Stmt>, ParseError)> {
        const ELEMENT: &'static str = "variable assignment";

        let (ident, r_ident) = match self.peek() {
            Token(Lexeme::Ident(s), r) => { self.advance(); (s.clone(), r) },
            Token(l, r) => return Err(expected(Item::Ident, Item::Lexeme(l), r).while_parsing(ELEMENT)),
        };

        let r_assign = match self.peek() {
            Token(Lexeme::Assign, r) => { self.advance(); r },
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::Assign), Item::Lexeme(l), r).while_parsing(ELEMENT)),
        };

        let (expr, max_err) = self.read_expr().map_err(|err| err.while_parsing(ELEMENT))?;

        match self.peek() {
            Token(Lexeme::Semicolon, r) => {
                self.advance();
                let r_union = expr.1.union(&r_ident).union(&r_assign).union(&r);
                Ok((Node(Stmt::Assign(Node(ident, r_ident), expr), r_union), max_err))
            },
            Token(l, r) => Err(expected(Item::Lexeme(Lexeme::Semicolon), Item::Lexeme(l), r).max(max_err).while_parsing(ELEMENT)),
        }
    }

    fn read_stmt(&mut self) -> ParseResult<(Node<Stmt>, ParseError)> {
        let mut this = self.clone();
        let max_err = match this.read_expr_stmt() {
            Ok((stmt, err)) => {
                *self = this;
                return Ok((stmt, err))
            },
            Err(err) => err,
        };

        let mut this = self.clone();
        let max_err = match this.read_print_stmt() {
            Ok((stmt, err)) => {
                *self = this;
                return Ok((stmt, err.max(max_err)))
            },
            Err(err) => err.max(max_err),
        };

        let mut this = self.clone();
        let max_err = match this.read_if_else_stmt() {
            Ok((stmt, err)) => {
                *self = this;
                return Ok((stmt, err.max(max_err)))
            },
            Err(err) => err.max(max_err),
        };

        let mut this = self.clone();
        let max_err = match this.read_while_stmt() {
            Ok((stmt, err)) => {
                *self = this;
                return Ok((stmt, err.max(max_err)))
            },
            Err(err) => err.max(max_err),
        };

        let mut this = self.clone();
        let max_err = match this.read_decl_stmt() {
            Ok((stmt, err)) => {
                *self = this;
                return Ok((stmt, err.max(max_err)))
            },
            Err(err) => err.max(max_err),
        };

        let mut this = self.clone();
        let max_err = match this.read_assign_stmt() {
            Ok((stmt, err)) => {
                *self = this;
                return Ok((stmt, err.max(max_err)))
            },
            Err(err) => err.max(max_err),
        };

        let mut this = self.clone();
        let max_err = match this.read_return_stmt() {
            Ok((stmt, err)) => {
                *self = this;
                return Ok((stmt, err.max(max_err)))
            },
            Err(err) => err.max(max_err),
        };

        let next = self.peek();
        Err(expected(Item::Stmt, Item::Lexeme(next.0), next.1).max(max_err))
    }

    fn read_stmts(&mut self) -> ParseResult<(Vec<Node<Stmt>>, ParseError)> {
        let mut stmts = vec![];

        let mut max_err = ParseError::phoney();

        loop {
            let mut this = self.clone();

            match this.read_stmt() { // TODO: Not this
                Ok((stmt, err)) => {
                    *self = this;
                    stmts.push(stmt);
                    max_err = err.max(max_err);
                },
                Err(err) => return Ok((stmts, err.max(max_err))),
            }
        }
    }

    fn read_block(&mut self) -> ParseResult<(Node<Block>, ParseError)> {
        let r_start = match self.peek() {
            Token(Lexeme::LBrace, r) => { self.advance(); r },
            Token(l, r) => return Err(expected(Item::Lexeme(Lexeme::LBrace), Item::Lexeme(l), r)),
        };

        let (stmts, max_err) = self.read_stmts()?;

        match self.peek() {
            Token(Lexeme::RBrace, r) => {
                self.advance();
                let r_union = stmts
                        .iter()
                        .fold(SrcRef::empty(), |r, p| p.1.union(&r))
                        .union(&r_start)
                        .union(&r);
                Ok((Node(Block(stmts), r_union), max_err))
            },
            Token(l, r) => Err(expected(Item::Lexeme(Lexeme::RBrace), Item::Lexeme(l), r).max(max_err)),
        }
    }

    fn read_args(&mut self) -> ParseResult<(Node<Args>, ParseError)> {
        let mut args = vec![];
        let mut r_total = SrcRef::empty();
        let mut max_err = ParseError::Phoney;

        loop {
            match self.peek() {
                Token(Lexeme::Ident(s), r) => {
                    self.advance();
                    r_total = r_total.union(&r);
                    args.push(Node(s.clone(), r));
                },
                Token(l, r) => {
                    max_err = expected(Item::Ident, Item::Lexeme(l), r).max(max_err);
                    break;
                },
            }

            match self.peek() {
                Token(Lexeme::Comma, r) => {
                    self.advance();
                    r_total = r_total.union(&r);
                },
                Token(l, r) => {
                    max_err = expected(Item::Lexeme(Lexeme::Comma), Item::Lexeme(l), r).max(max_err);
                    break;
                },
            }
        }

        Ok((Node(Args(args), r_total), max_err))
    }

    pub fn read_expr_full(&mut self) -> ParseResult<Expr> {
        let (expr, max_err) = match self.read_expr() {
            Ok((expr, max_err)) => (expr, max_err),
            Err(err) => return match self.peek() {
                Token(Lexeme::Eof, _) => Ok(Expr::None),
                _ => Err(err),
            },
        };
        match self.peek() {
            Token(Lexeme::Eof, _) => Ok(expr.0),
            Token(l, r) => Err(expected(Item::End, Item::Lexeme(l), r).max(max_err)),
        }
    }

    pub fn read_stmts_full(&mut self) -> ParseResult<Vec<Node<Stmt>>> {
        let (stmts, max_err) = match self.read_stmts() {
            Ok((stmts, max_err)) => (stmts, max_err),
            Err(err) => return match self.peek() {
                Token(Lexeme::Eof, _) => Ok(vec![]),
                _ => Err(err),
            },
        };
        match self.peek() {
            Token(Lexeme::Eof, _) => Ok(stmts),
            Token(l, r) => Err(expected(Item::End, Item::Lexeme(l), r).max(max_err)),
        }
    }
}
