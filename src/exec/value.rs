use std::{
    rc::Rc,
    cmp::PartialEq,
    fmt,
    ops::Range,
    convert::TryFrom,
};
use crate::{
    parser::{
        SrcRef,
        ast::{
            Node,
            Args,
            Block,
            Expr,
        },
    },
};
use super::{
    Obj,
    UnaryOpRef,
    BinaryOpRef,
    ExecError,
    ExecResult,
    Scope,
    GlobalScope,
    Io,
};

pub trait ForgeIter = Iterator<Item=Value> + fmt::Debug;

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Char(char),
    Boolean(bool),
    Range(f64, f64),
    Fn(Rc<String>, Rc<(Node<Args>, Node<Block>)>),
    List(Rc<Vec<Value>>),
    Custom(Rc<dyn Obj>),
    Null,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(x) => writeln!(f, "Number({:?})", x),
            Value::String(s) => writeln!(f, "String({:?})", s),
            Value::Char(c) => writeln!(f, "Char({:?})", c),
            Value::Boolean(b) => writeln!(f, "Boolean({:?})", b),
            Value::Range(x, y) => writeln!(f, "Range({:?}, {:?})", x, y),
            Value::Fn(s, func) => writeln!(f, "Fn({:?}, {:?})", s, func),
            Value::List(l) => writeln!(f, "List({:?})", l),
            Value::Custom(c) => writeln!(f, "Custom({:?})", &c as *const _),
            Value::Null => writeln!(f, "Null"),
        }
    }
}

impl<V: Into<Value>, F: Fn() -> V + fmt::Debug + 'static> Obj for F {
    fn eval_call(&self, params: &Node<Vec<Node<Expr>>>, _caller: &mut dyn Scope, _io: &mut dyn Io, src: &Rc<String>, _r_caller: SrcRef) -> ExecResult<Value> {
        if params.0.len() != 0 {
            Err(ExecError::At(params.1, Box::new(ExecError::WrongArgNum(
                None, 0, params.0.len()
            )))).map_err(|err| ExecError::WithSrc(src.clone(), Box::new(err)))
        } else {
            Ok(self.call(()).into())
        }
    }
}

/* TODO: Resolve conflicting impl
impl<V: Into<Value>, F: Fn(Value) -> V + fmt::Debug + 'static> Obj for F {
    fn eval_call(&self, params: &Node<Vec<Node<Expr>>>, _caller: &mut dyn Scope, _io: &mut dyn Io, src: &Rc<String>, _r_caller: SrcRef) -> ExecResult<Value> {
        if params.0.len() != 1 {
            Err(ExecError::At(params.1, Box::new(ExecError::WrongArgNum(
                None, 0, params.0.len()
            )))).map_err(|err| ExecError::WithSrc(src.clone(), Box::new(err)))
        } else {
            Ok(self.call((self.params.0[0]))
        }
    }
}
*/

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(x), Value::Number(y)) => x.eq(y),
            (Value::String(x), Value::String(y)) => x.eq(y),
            (Value::Char(x), Value::Char(y)) => x.eq(y),
            (Value::Boolean(x), Value::Boolean(y)) => x.eq(y),
            (Value::Range(x0, x1), Value::Range(y0, y1)) => (x0, x1).eq(&(y0, y1)),
            (Value::Fn(_, x), Value::Fn(_, y)) => Rc::ptr_eq(&x, &y),
            (Value::List(x), Value::List(y)) => Rc::ptr_eq(&x, &y),
            (Value::Null, Value::Null) => true,
            _ => false,
        }
    }
}

impl Value {
    pub fn as_custom(self) -> Option<Rc<dyn Obj>> {
        match self {
            Value::Custom(rc) => Some(rc.clone()),
            _ => None,
        }
    }

    pub fn call(&self, params: &Node<Vec<Node<Expr>>>, caller: &mut dyn Scope, io: &mut dyn Io, src: &Rc<String>, r_caller: SrcRef) -> ExecResult<Value> {
        match self {
            Value::Fn(code, f) => if ((f.0).0).0.len() != params.0.len() {
                return Err(ExecError::WithPrevSrc(code.clone(), Box::new(ExecError::At(params.1, Box::new(ExecError::WrongArgNum(
                    Some((f.0).1), ((f.0).0).0.len(), params.0.len()
                )))))).map_err(|err| ExecError::WithSrc(src.clone(), Box::new(err)));
            } else {
                // TODO: Properly scope functions
                let mut scope = GlobalScope::empty();
                for (arg, param) in ((f.0).0).0.iter().zip(&params.0) {
                    scope.declare_var(arg.0.clone(), caller.eval_expr(&param.0, io, src)?);
                }
                Ok(scope.eval_block(&(f.1).0, io, &code)?.unwrap_or(Value::Null))
            },
            Value::Custom(custom) => custom.eval_call(params, caller, io, src, r_caller),
            _ => Err(ExecError::At(r_caller, Box::new(ExecError::CannotCall(self.get_type_name())))),
        }
    }

    pub fn get_type_name(&self) -> String {
        match self {
            Value::Number(_) => String::from("number"),
            Value::String(_) => String::from("string"),
            Value::Char(_) => String::from("char"),
            Value::Boolean(_) => String::from("bool"),
            Value::Range(_, _) => String::from("range"),
            Value::Fn(_, _) => String::from("function"),
            Value::List(_) => String::from("list"),
            Value::Custom(c) => c.get_type_name(),
            Value::Null => String::from("null"),
        }
    }

    pub fn get_display_text(&self) -> ExecResult<String> {
        Ok(match self {
            Value::Number(x) => format!("{}", x),
            Value::String(s) => s.clone(),
            Value::Char(c) => format!("{}", c),
            Value::Boolean(b) => format!("{}", b),
            Value::Range(x, y) => format!("{}..{}", x, y),
            Value::Fn(_, _) => String::from("<function>"),
            Value::List(l) => {
                let mut s = String::from("[");
                if let Some(i) = l.get(0) {
                    s += &i.get_display_text()?;
                }
                for item in l.get(1..).unwrap_or(&[]) {
                    s += ", ";
                    s += &item.get_display_text()?;
                }
                s.push(']');
                s
            },
            Value::Custom(c) => c.get_display_text()?,
            Value::Null => String::from("<null>"),
        })
    }

    pub fn eval_truth(&self, r: SrcRef) -> ExecResult<bool> {
        match self {
            Value::Boolean(b) => Ok(*b),
            Value::Custom(c) => c.eval_truth(r),
            _ => Err(ExecError::CannotDetermineTruthiness(r, self.get_type_name())),
        }
    }

    pub fn eval_not(&self, refs: UnaryOpRef) -> ExecResult<Value> {
        match self {
            Value::Boolean(b) => Ok(Value::Boolean(!b)),
            Value::Custom(c) => c.eval_not(refs),
            _ => Err(ExecError::UnaryOp {
                op: "not",
                expr_type: self.get_type_name(),
                refs,
            })
        }
    }

    pub fn eval_neg(&self, refs: UnaryOpRef) -> ExecResult<Value> {
        match self {
            Value::Number(x) => Ok(Value::Number(-x)),
            Value::Custom(c) => c.eval_neg(refs),
            _ => Err(ExecError::UnaryOp {
                op: "not",
                expr_type: self.get_type_name(),
                refs,
            })
        }
    }

    pub fn eval_clone(&self, refs: UnaryOpRef) -> ExecResult<Value> {
        match self {
            Value::Number(x) => Ok(Value::Number(*x)),
            Value::String(s) => Ok(Value::String(s.clone())),
            Value::Char(c) => Ok(Value::Char(*c)),
            Value::Boolean(b) => Ok(Value::Boolean(*b)),
            Value::Range(x, y) => Ok(Value::Range(*x, *y)),
            Value::Fn(s, f) => Ok(Value::Fn(s.clone(), f.clone())),
            Value::List(l) => Ok(Value::List(Rc::new(l.as_ref().clone()))),
            Value::Custom(c) => c.eval_clone(refs),
            Value::Null => Ok(Value::Null),
        }
    }

    pub fn eval_mirror(&self, refs: UnaryOpRef) -> ExecResult<Value> {
        match self {
            Value::Number(x) => Ok(Value::Number(*x)),
            Value::String(s) => Ok(Value::String(s.clone())),
            Value::Char(c) => Ok(Value::Char(*c)),
            Value::Boolean(b) => Ok(Value::Boolean(*b)),
            Value::Range(x, y) => Ok(Value::Range(*x, *y)),
            Value::Fn(s, f) => Ok(Value::Fn(s.clone(), f.clone())),
            Value::List(l) => Ok(Value::List(Rc::new(l.as_ref().iter().map(|i| i.eval_mirror(refs)).collect::<Result<_, _>>()?))),
            Value::Custom(c) => c.eval_mirror(refs),
            Value::Null => Ok(Value::Null),
        }
    }

    pub fn eval_mul(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(*x * *y)),
            (Value::Custom(c), rhs) => c.eval_mul(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "mul",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    pub fn eval_div(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(*x / *y)),
            (Value::Custom(c), rhs) => c.eval_div(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "div",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    pub fn eval_mod(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(*x % *y)),
            (Value::Custom(c), rhs) => c.eval_mod(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "mod",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    pub fn eval_add(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x.clone() + y)),
            (Value::String(x), Value::String(y)) => Ok(Value::String(x.clone() + y)),
            (Value::String(x), Value::Char(y)) => Ok(Value::String(format!("{}{}", x, y))),
            (Value::String(x), Value::Number(y)) => Ok(Value::String(x.clone() + &format!("{}", y))),
            (Value::String(x), Value::Boolean(y)) => Ok(Value::String(x.clone() + &format!("{}", y))),
            (Value::String(x), Value::Null) => Ok(Value::String(x.clone() + &"null")),
            (Value::List(x), Value::List(y)) => {
                let mut v = x.as_ref().clone();
                v.append(&mut y.as_ref().clone());
                Ok(Value::List(Rc::new(v)))
            },
            (Value::List(x), rhs) => {
                let mut v = x.as_ref().clone();
                v.push(rhs.clone());
                Ok(Value::List(Rc::new(v)))
            },
            (Value::Custom(c), rhs) => c.eval_add(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "add",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    pub fn eval_sub(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(*x - *y)),
            (Value::Custom(c), rhs) => c.eval_sub(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "sub",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    pub fn eval_greater(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x > *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x > *y)),
            (Value::Char(x), Value::Char(y)) => Ok(Value::Boolean(*x > *y)),
            (Value::Custom(c), rhs) => c.eval_greater(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "greater",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    pub fn eval_greater_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x >= *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x >= *y)),
            (Value::Char(x), Value::Char(y)) => Ok(Value::Boolean(*x >= *y)),
            (Value::Custom(c), rhs) => c.eval_greater_eq(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "greater_eq",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    pub fn eval_less(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x < *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x < *y)),
            (Value::Char(x), Value::Char(y)) => Ok(Value::Boolean(*x < *y)),
            (Value::Custom(c), rhs) => c.eval_less(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "less",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    pub fn eval_less_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x <= *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x <= *y)),
            (Value::Char(x), Value::Char(y)) => Ok(Value::Boolean(*x <= *y)),
            (Value::Custom(c), rhs) => c.eval_less_eq(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "less_eq",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    pub fn eval_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x == *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x == *y)),
            (Value::Char(x), Value::Char(y)) => Ok(Value::Boolean(*x == *y)),
            (Value::Boolean(x), Value::Boolean(y)) => Ok(Value::Boolean(*x == *y)),
            (Value::Fn(_, x), Value::Fn(_, y)) => Ok(Value::Boolean(Rc::ptr_eq(&x, &y))),
            (Value::Custom(c), rhs) => c.eval_eq(rhs, refs),
            (Value::Null, Value::Null) => Ok(Value::Boolean(true)),
            _ => Ok(Value::Boolean(false)),
        }
    }

    pub fn eval_not_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x != *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x != *y)),
            (Value::Char(x), Value::Char(y)) => Ok(Value::Boolean(*x != *y)),
            (Value::Boolean(x), Value::Boolean(y)) => Ok(Value::Boolean(*x != *y)),
            (Value::Fn(_, x), Value::Fn(_, y)) => Ok(Value::Boolean(!Rc::ptr_eq(&x, &y))),
            (Value::Custom(c), rhs) => c.eval_not_eq(rhs, refs),
            (Value::Null, Value::Null) => Ok(Value::Boolean(false)),
            _ => Ok(Value::Boolean(true)),
        }
    }

    pub fn eval_and(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Boolean(x), Value::Boolean(y)) => Ok(Value::Boolean(*x && *y)),
            (Value::Custom(c), rhs) => c.eval_and(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "and",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    pub fn eval_or(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Boolean(x), Value::Boolean(y)) => Ok(Value::Boolean(*x || *y)),
            (Value::Custom(c), rhs) => c.eval_or(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "or",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    pub fn eval_xor(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Boolean(x), Value::Boolean(y)) => Ok(Value::Boolean(*x ^ *y)),
            (Value::Custom(c), rhs) => c.eval_xor(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "xor",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    pub fn eval_range(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Range(*x, *y)),
            (Value::Custom(c), rhs) => c.eval_range(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "range",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    pub fn eval_iter(&self, r: SrcRef) -> ExecResult<Box<ForgeIter>> {
        match self {
            Value::Range(x, y) => Ok(Box::new((*x as i64..*y as i64).map(|v| Value::Number(v as f64)))),
            Value::String(s) => Ok(Box::new(s.chars().collect::<Vec<_>>().into_iter().map(|c| Value::Char(c)))),
            Value::List(l) => Ok(Box::new(l.as_ref().clone().into_iter())),
            Value::Custom(c) => c.eval_iter(r),
            _ => Err(ExecError::At(r, Box::new(ExecError::NotIterable(self.get_type_name())))),
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

impl PartialEq<i64> for Value {
    fn eq(&self, other: &i64) -> bool {
        match self {
            Value::Number(x) => x.eq(&(*other as f64)),
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.get_display_text().unwrap_or("<cannot display value>".to_string()))
    }
}

impl<T: Obj> From<T> for Value {
    fn from(other: T) -> Self {
        Value::Custom(Rc::new(other))
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Null
    }
}

impl From<f64> for Value {
    fn from(other: f64) -> Self {
        Value::Number(other)
    }
}

impl From<i64> for Value {
    fn from(other: i64) -> Self {
        Value::Number(other as f64)
    }
}

impl From<bool> for Value {
    fn from(other: bool) -> Self {
        Value::Boolean(other)
    }
}

impl<'a> From<&'a str> for Value {
    fn from(other: &'a str) -> Self {
        Value::String(other.to_string())
    }
}

impl From<String> for Value {
    fn from(other: String) -> Self {
        Value::String(other)
    }
}

impl From<Range<i64>> for Value {
    fn from(other: Range<i64>) -> Self {
        Value::Range(other.start as f64, other.end as f64)
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(other: Vec<T>) -> Self {
        Value::List(Rc::new(other.into_iter().map(|i| i.into()).collect()))
    }
}
