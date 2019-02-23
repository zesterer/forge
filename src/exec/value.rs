use std::rc::Rc;
use crate::parser::ast::{
    Node,
    Args,
    Block,
    Function,
};
use super::{
    Obj,
    UnaryOpRef,
    BinaryOpRef,
    ExecError,
    ExecResult,
};

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Fn(Rc<Node<Function>>),
    Null,
}

impl Obj for Value {
    fn get_type_string(&self) -> String {
        match self {
            Value::Number(x) => String::from("number"),
            Value::String(s) => String::from("string"),
            Value::Boolean(b) => String::from("bool"),
            Value::Fn(_) => String::from("function"),
            Value::Null => String::from("null"),
        }
    }

    fn eval_not(&self, unary_op_ref: UnaryOpRef) -> ExecResult<Value> {
        match self {
            Value::Boolean(b) => Ok(Value::Boolean(!b)),
            _ => Err(ExecError::UnaryOp {
                op: "not",
                expr_type: self.get_type_string(),
                unary_op_ref,
            })
        }
    }

    fn eval_neg(&self, unary_op_ref: UnaryOpRef) -> ExecResult<Value> {
        match self {
            Value::Number(x) => Ok(Value::Number(-x)),
            _ => Err(ExecError::UnaryOp {
                op: "not",
                expr_type: self.get_type_string(),
                unary_op_ref,
            })
        }
    }

    fn eval_mul(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(*x * *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "mul",
                left_type: this.get_type_string(),
                right_type: rhs.get_type_string(),
                binary_op_ref,
            }),
        }
    }

    fn eval_div(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(*x / *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "div",
                left_type: this.get_type_string(),
                right_type: rhs.get_type_string(),
                binary_op_ref,
            }),
        }
    }

    fn eval_add(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x.clone() + y)),
            (Value::String(x), Value::String(y)) => Ok(Value::String(x.clone() + y)),
            (Value::String(x), Value::Number(y)) => Ok(Value::String(x.clone() + &format!("{}", y))),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "add",
                left_type: this.get_type_string(),
                right_type: rhs.get_type_string(),
                binary_op_ref,
            }),
        }
    }

    fn eval_sub(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(*x - *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "sub",
                left_type: this.get_type_string(),
                right_type: rhs.get_type_string(),
                binary_op_ref,
            }),
        }
    }

    fn eval_greater(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x > *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x > *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "greater",
                left_type: this.get_type_string(),
                right_type: rhs.get_type_string(),
                binary_op_ref,
            }),
        }
    }

    fn eval_greater_eq(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x >= *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x >= *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "greater_eq",
                left_type: this.get_type_string(),
                right_type: rhs.get_type_string(),
                binary_op_ref,
            }),
        }
    }

    fn eval_less(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x < *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x < *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "less",
                left_type: this.get_type_string(),
                right_type: rhs.get_type_string(),
                binary_op_ref,
            }),
        }
    }

    fn eval_less_eq(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x <= *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x <= *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "less_eq",
                left_type: this.get_type_string(),
                right_type: rhs.get_type_string(),
                binary_op_ref,
            }),
        }
    }

    fn eval_eq(&self, rhs: &Value, binary_op_ref: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x == *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x == *y)),
            (Value::Boolean(x), Value::Boolean(y)) => Ok(Value::Boolean(*x == *y)),
            (Value::Fn(x), Value::Fn(y)) => Ok(Value::Boolean(Rc::ptr_eq(&x, &y))),
            (Value::Null, Value::Null) => Ok(Value::Boolean(true)),
            _ => Ok(Value::Boolean(false)),
        }
    }
}
