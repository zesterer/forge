use super::SrcRef;

#[derive(Debug)]
pub struct Node<T>(pub T, pub SrcRef);

#[derive(Debug)]
pub enum Expr {
    None,
    LiteralNumber(f64),
    LiteralString(String),
    LiteralBoolean(bool),
    LiteralNull,
    Ident(Node<String>),
    DotAccess(SrcRef, Box<Node<Expr>>, Node<String>),
    Call(SrcRef, Box<Node<Expr>>, Vec<Node<Expr>>),
    UnaryNot(SrcRef, Box<Node<Expr>>),
    UnaryNeg(SrcRef, Box<Node<Expr>>),
    BinaryMul(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
    BinaryDiv(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
    BinaryMod(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
    BinaryAdd(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
    BinarySub(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
    BinaryGreater(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
    BinaryGreaterEq(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
    BinaryLess(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
    BinaryLessEq(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
    BinaryEq(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
    BinaryNotEq(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
    BinaryAnd(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
    BinaryOr(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
    BinaryXor(SrcRef, Box<Node<Expr>>, Box<Node<Expr>>),
}

#[derive(Debug)]
pub struct Args; // TODO

#[derive(Debug)]
pub struct Block(pub Vec<Node<Stmt>>);

#[derive(Debug)]
pub struct Function {
    args: Node<Args>,
    block: Node<Block>,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Node<Expr>),
    Print(Node<Expr>),
    If(Node<Expr>, Node<Block>),
    IfElse(Node<Expr>, Node<Block>, Node<Block>),
    While(Node<Expr>, Node<Block>),
    Decl(Node<String>, Node<Expr>),
    Assign(Node<String>, Node<Expr>),
}

// Utility

struct Spaces(usize);

impl std::fmt::Display for Spaces {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for _ in 0..self.0 * 2 {
            let _ = write!(f, " ");
        }
        Ok(())
    }
}

impl Expr {
    pub fn print_debug(&self, depth: usize) {
        match self {
            Expr::None => println!("{}None expression", Spaces(depth)),
            Expr::LiteralNumber(x) => println!("{}Number literal '{}'", Spaces(depth), x),
            Expr::LiteralString(s) => println!("{}String literal '{}'", Spaces(depth), s),
            Expr::LiteralBoolean(b) => println!("{}Boolean literal '{}'", Spaces(depth), b),
            Expr::LiteralNull => println!("{}Null literal", Spaces(depth)),
            Expr::Ident(s) => println!("{}Identifier '{}'", Spaces(depth), s.0),
            Expr::DotAccess(_, expr, s) => {
                println!("{}Dot accessor '{}'", Spaces(depth), s.0);
                expr.0.print_debug(depth + 1);
            },
            Expr::Call(_, expr, params) => {
                println!("{}Call", Spaces(depth));
                expr.0.print_debug(depth + 1);
                for param in params {
                    println!("{}Parameter", Spaces(depth + 1));
                    param.0.print_debug(depth + 1);
                }
            },
            Expr::UnaryNot(_, expr) => {
                println!("{}Unary not", Spaces(depth));
                expr.0.print_debug(depth + 1);
            },
            Expr::UnaryNeg(_, expr) => {
                println!("{}Unary neg", Spaces(depth));
                expr.0.print_debug(depth + 1);
            },
            Expr::BinaryMul(_, left, right) => {
                println!("{}Binary mul", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
            Expr::BinaryDiv(_, left, right) => {
                println!("{}Binary div", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
            Expr::BinaryMod(_, left, right) => {
                println!("{}Binary mod", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
            Expr::BinaryAdd(_, left, right) => {
                println!("{}Binary add", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
            Expr::BinarySub(_, left, right) => {
                println!("{}Binary sub", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
            Expr::BinaryGreater(_, left, right) => {
                println!("{}Binary greater", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
            Expr::BinaryGreaterEq(_, left, right) => {
                println!("{}Binary greater_eq", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
            Expr::BinaryLess(_, left, right) => {
                println!("{}Binary less", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
            Expr::BinaryLessEq(_, left, right) => {
                println!("{}Binary less_eq", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
            Expr::BinaryEq(_, left, right) => {
                println!("{}Binary eq", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
            Expr::BinaryNotEq(_, left, right) => {
                println!("{}Binary eq", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
            Expr::BinaryAnd(_, left, right) => {
                println!("{}Binary and", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
            Expr::BinaryOr(_, left, right) => {
                println!("{}Binary or", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
            Expr::BinaryXor(_, left, right) => {
                println!("{}Binary xor", Spaces(depth));
                left.0.print_debug(depth + 1);
                right.0.print_debug(depth + 1);
            },
        }
    }
}

impl Stmt {
    pub fn print_debug(&self, depth: usize) {
        match self {
            Stmt::Expr(expr) => {
                println!("{}Expression statement", Spaces(depth));
                expr.0.print_debug(depth + 1);
            },
            Stmt::Print(expr) => {
                println!("{}Print statement", Spaces(depth));
                expr.0.print_debug(depth + 1);
            },
            Stmt::If(expr, block) => {
                println!("{}If statement", Spaces(depth));
                expr.0.print_debug(depth + 1);
                block.0.print_debug(depth + 1);
            },
            Stmt::IfElse(expr, true_block, false_block) => {
                println!("{}If-else statement", Spaces(depth));
                expr.0.print_debug(depth + 1);
                true_block.0.print_debug(depth + 1);
                false_block.0.print_debug(depth + 1);
            },
            Stmt::While(expr, block) => {
                println!("{}While statement", Spaces(depth));
                expr.0.print_debug(depth + 1);
                block.0.print_debug(depth + 1);
            },
            Stmt::Decl(ident, expr) => {
                println!("{}Declaration statement '{}'", Spaces(depth), ident.0);
                expr.0.print_debug(depth + 1);
            },
            Stmt::Assign(ident, expr) => {
                println!("{}Assignment statement '{}'", Spaces(depth), ident.0);
                expr.0.print_debug(depth + 1);
            },
        }
    }
}

impl Block {
    pub fn print_debug(&self, depth: usize) {
        println!("{}Block", Spaces(depth));
        for stmt in &self.0 {
            stmt.0.print_debug(depth + 2);
        }
    }
}
