use std::{
    rc::Rc,
    cmp::PartialEq,
};
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

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(x), Value::Number(y)) => x.eq(y),
            (Value::String(x), Value::String(y)) => x.eq(y),
            (Value::Boolean(x), Value::Boolean(y)) => x.eq(y),
            (Value::Fn(x), Value::Fn(y)) => Rc::ptr_eq(x, y),
            (Value::Null, Value::Null) => true,
            _ => false,
        }
    }
}

impl PartialEq<f64> for Value {
    fn eq(&self, other: &f64) -> bool {
        match self {
            Value::Number(x) => x.eq(other),
            _ => false,
        }
    }
}

impl<'a> PartialEq<&'a str> for Value {
    fn eq(&self, other: &&'a str) -> bool {
        match self {
            Value::String(x) => x.eq(*other),
            _ => false,
        }
    }
}

impl PartialEq<bool> for Value {
    fn eq(&self, other: &bool) -> bool {
        match self {
            Value::Boolean(x) => x.eq(other),
            _ => false,
        }
    }
}

impl Obj for Value {
    fn get_type_name(&self) -> String {
        match self {
            Value::Number(x) => String::from("number"),
            Value::String(s) => String::from("string"),
            Value::Boolean(b) => String::from("bool"),
            Value::Fn(_) => String::from("function"),
            Value::Null => String::from("null"),
        }
    }

    fn get_display_text(&self) -> String {
        match self {
            Value::Number(x) => format!("{}", x),
            Value::String(s) => s.clone(),
            Value::Boolean(b) => format!("{}", b),
            Value::Fn(_) => String::from("<function>"),
            Value::Null => String::from("<null>"),
        }
    }

    fn eval_not(&self, refs: UnaryOpRef) -> ExecResult<Value> {
        match self {
            Value::Boolean(b) => Ok(Value::Boolean(!b)),
            _ => Err(ExecError::UnaryOp {
                op: "not",
                expr_type: self.get_type_name(),
                refs,
            })
        }
    }

    fn eval_neg(&self, refs: UnaryOpRef) -> ExecResult<Value> {
        match self {
            Value::Number(x) => Ok(Value::Number(-x)),
            _ => Err(ExecError::UnaryOp {
                op: "not",
                expr_type: self.get_type_name(),
                refs,
            })
        }
    }

    fn eval_mul(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(*x * *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "mul",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    fn eval_div(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(*x / *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "div",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    fn eval_add(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x.clone() + y)),
            (Value::String(x), Value::String(y)) => Ok(Value::String(x.clone() + y)),
            (Value::String(x), Value::Number(y)) => Ok(Value::String(x.clone() + &format!("{}", y))),
            (Value::String(x), Value::Boolean(y)) => Ok(Value::String(x.clone() + &format!("{}", y))),
            (Value::String(x), Value::Null) => Ok(Value::String(x.clone() + &"null")),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "add",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    fn eval_sub(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(*x - *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "sub",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    fn eval_greater(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x > *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x > *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "greater",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    fn eval_greater_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x >= *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x >= *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "greater_eq",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    fn eval_less(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x < *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x < *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "less",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    fn eval_less_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x <= *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x <= *y)),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "less_eq",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    fn eval_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x == *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x == *y)),
            (Value::Boolean(x), Value::Boolean(y)) => Ok(Value::Boolean(*x == *y)),
            (Value::Fn(x), Value::Fn(y)) => Ok(Value::Boolean(Rc::ptr_eq(&x, &y))),
            (Value::Null, Value::Null) => Ok(Value::Boolean(true)),
            _ => Ok(Value::Boolean(false)),
        }
    }

    fn eval_not_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x != *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x != *y)),
            (Value::Boolean(x), Value::Boolean(y)) => Ok(Value::Boolean(*x != *y)),
            (Value::Fn(x), Value::Fn(y)) => Ok(Value::Boolean(!Rc::ptr_eq(&x, &y))),
            (Value::Null, Value::Null) => Ok(Value::Boolean(false)),
            _ => Ok(Value::Boolean(true)),
        }
    }
}
