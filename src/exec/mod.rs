mod value;

// Reexports
pub use self::value::Value;

use crate::parser::{
    SrcRef,
    ast::{
        Node,
        Expr,
        Args,
        Block,
    },
};

#[derive(Debug)]
pub enum ExecError {
    UnaryOp {
        op: &'static str,
        expr_type: String,
        unary_op_ref: UnaryOpRef,
    },
    BinaryOp {
        op: &'static str,
        left_type: String,
        right_type: String,
        binary_op_ref: BinaryOpRef,
    },
}

pub type ExecResult<T> = Result<T, ExecError>;

pub trait Io {}

pub struct DefaultIo;

impl Io for DefaultIo {}

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
    fn get_type_string(&self) -> String;

    fn eval_not(&self, unary_op_ref: UnaryOpRef) -> ExecResult<Value> {
        Err(ExecError::UnaryOp {
            op: "not",
            expr_type: self.get_type_string(),
            unary_op_ref,
        })
    }

    fn eval_neg(&self, unary_op_ref: UnaryOpRef) -> ExecResult<Value> {
        Err(ExecError::UnaryOp {
            op: "neg",
            expr_type: self.get_type_string(),
            unary_op_ref,
        })
    }

    fn eval_mul(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "mul",
            left_type: self.get_type_string(),
            right_type: self.get_type_string(),
            binary_op_ref,
        })
    }

    fn eval_div(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "div",
            left_type: self.get_type_string(),
            right_type: self.get_type_string(),
            binary_op_ref,
        })
    }

    fn eval_add(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "add",
            left_type: self.get_type_string(),
            right_type: self.get_type_string(),
            binary_op_ref,
        })
    }

    fn eval_sub(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "sub",
            left_type: self.get_type_string(),
            right_type: self.get_type_string(),
            binary_op_ref,
        })
    }

    fn eval_greater(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "greater",
            left_type: self.get_type_string(),
            right_type: self.get_type_string(),
            binary_op_ref,
        })
    }

    fn eval_greater_eq(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "greater_eq",
            left_type: self.get_type_string(),
            right_type: self.get_type_string(),
            binary_op_ref,
        })
    }

    fn eval_less(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "less",
            left_type: self.get_type_string(),
            right_type: self.get_type_string(),
            binary_op_ref,
        })
    }

    fn eval_less_eq(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "less_eq",
            left_type: self.get_type_string(),
            right_type: self.get_type_string(),
            binary_op_ref,
        })
    }

    fn eval_eq(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "eq",
            left_type: self.get_type_string(),
            right_type: self.get_type_string(),
            binary_op_ref,
        })
    }

    fn eval_not_eq(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "not_eq",
            left_type: self.get_type_string(),
            right_type: self.get_type_string(),
            binary_op_ref,
        })
    }
}

pub trait Scope {
    fn eval_expr(&self, expr: &Expr, io: &mut dyn Io) -> ExecResult<Value> {
        match expr {
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
            _ => unimplemented!(),
        }
    }
}
