mod value;

// Reexports
pub use self::value::Value;

use std::fmt;
use crate::{
    output,
    parser::{
        SrcRef,
        ast::{
            Node,
            Expr,
            Stmt,
            Args,
            Block,
        },
    },
};

#[derive(Debug)]
pub enum ExecError {
    UnaryOp {
        op: &'static str,
        expr_type: String,
        refs: UnaryOpRef,
    },
    BinaryOp {
        op: &'static str,
        left_type: String,
        right_type: String,
        refs: BinaryOpRef,
    },
}

impl ExecError {
    pub fn fmt_nice(&self, f: &mut fmt::Formatter, src: Option<&str>, depth: usize) -> fmt::Result {
        match self {
            ExecError::UnaryOp { op, expr_type, refs } => {
                Ok(())
                    .and_then(|_| writeln!(f, "Runtime error at {}...", refs.op.start()))
                    .and_then(|_| output::fmt_ref(f, refs.op, src, depth + 1))
                    .and_then(|_| output::fmt_ref(f, refs.expr, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Cannot apply unary operator '{}' to value of type '{}'.", output::Repeat(' ', (depth + 1) * 3), op, expr_type))
            },
            ExecError::BinaryOp { op, left_type, right_type, refs } => {
                Ok(())
                    .and_then(|_| writeln!(f, "Runtime error at {}:", refs.op.start()))
                    .and_then(|_| output::fmt_ref(f, refs.left, src, depth + 1))
                    .and_then(|_| output::fmt_ref(f, refs.right, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Cannot apply binary operator '{}' to values of types '{}' and '{}'.", output::Repeat(' ', (depth + 1) * 3), op, left_type, right_type))
            },
        }
    }
}

pub type ExecResult<T> = Result<T, ExecError>;

pub trait Io {
    fn print(&self, s: String) -> ExecResult<()>;
}

pub struct DefaultIo;

impl Io for DefaultIo {
    fn print(&self, s: String) -> ExecResult<()> {
        println!("{}", s);
        Ok(())
    }
}

#[derive(Debug)]
pub struct UnaryOpRef {
    op: SrcRef,
    expr: SrcRef,
}

#[derive(Debug)]
pub struct BinaryOpRef {
    op: SrcRef,
    left: SrcRef,
    right: SrcRef,
}

pub trait Obj: Sized {
    fn get_type_name(&self) -> String;

    fn get_display_text(&self) -> String;

    fn eval_not(&self, refs: UnaryOpRef) -> ExecResult<Value> {
        Err(ExecError::UnaryOp {
            op: "not",
            expr_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_neg(&self, refs: UnaryOpRef) -> ExecResult<Value> {
        Err(ExecError::UnaryOp {
            op: "neg",
            expr_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_mul(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "mul",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_div(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "div",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_add(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "add",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_sub(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "sub",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_greater(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "greater",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_greater_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "greater_eq",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_less(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "less",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_less_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "less_eq",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "eq",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_not_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "not_eq",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }
}

pub trait Scope {
    fn eval_expr(&self, expr: &Expr, io: &mut dyn Io) -> ExecResult<Value> {
        match expr {
            Expr::None => Ok(Value::Null),
            Expr::LiteralNumber(x) => Ok(Value::Number(*x)),
            Expr::LiteralString(s) => Ok(Value::String(s.to_string())),
            Expr::LiteralBoolean(b) => Ok(Value::Boolean(*b)),
            Expr::LiteralNull => Ok(Value::Null),
            Expr::Ident(_) => unimplemented!(),
            Expr::DotAccess(_, _, _) => unimplemented!(),
            Expr::Call(_, _, _) => unimplemented!(),
            Expr::UnaryNot(r, expr) =>
                self.eval_expr(&expr.0, io)?.eval_not(UnaryOpRef { op: *r, expr: expr.1 }),
            Expr::UnaryNeg(r, expr) =>
                self.eval_expr(&expr.0, io)?.eval_neg(UnaryOpRef { op: *r, expr: expr.1 }),
            Expr::BinaryMul(r, left, right) =>
                self.eval_expr(&left.0, io)?.eval_mul(&self.eval_expr(&right.0, io)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryDiv(r, left, right) =>
                self.eval_expr(&left.0, io)?.eval_div(&self.eval_expr(&right.0, io)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryAdd(r, left, right) =>
                self.eval_expr(&left.0, io)?.eval_add(&self.eval_expr(&right.0, io)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinarySub(r, left, right) =>
                self.eval_expr(&left.0, io)?.eval_sub(&self.eval_expr(&right.0, io)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryGreater(r, left, right) =>
                self.eval_expr(&left.0, io)?.eval_greater(&self.eval_expr(&right.0, io)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryGreaterEq(r, left, right) =>
                self.eval_expr(&left.0, io)?.eval_greater_eq(&self.eval_expr(&right.0, io)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryLess(r, left, right) =>
                self.eval_expr(&left.0, io)?.eval_less(&self.eval_expr(&right.0, io)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryLessEq(r, left, right) =>
                self.eval_expr(&left.0, io)?.eval_less_eq(&self.eval_expr(&right.0, io)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryEq(r, left, right) =>
                self.eval_expr(&left.0, io)?.eval_eq(&self.eval_expr(&right.0, io)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryNotEq(r, left, right) =>
                self.eval_expr(&left.0, io)?.eval_not_eq(&self.eval_expr(&right.0, io)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
        }
    }

    fn eval_stmt(&self, stmt: &Stmt, io: &mut dyn Io) -> ExecResult<()> {
        match stmt {
            Stmt::Expr(expr) => { self.eval_expr(&expr.0, io)?; Ok(()) },
            Stmt::Print(expr) => {
                let text = self.eval_expr(&expr.0, io)?.get_display_text();
                io.print(text)
            },
        }
    }
}
