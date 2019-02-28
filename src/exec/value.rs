use std::{
    rc::Rc,
    cmp::PartialEq,
    fmt,
    ops::Range,
    cell::RefCell,
    collections::HashMap as StdHashMap,
    hash::{Hash, Hasher},
    mem,
};
use::hashbrown::HashMap;
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

#[derive(Debug)]
pub enum Type {
    Number,
    String,
    Char,
    Boolean,
    Range,
    Fn,
    List,
    Custom,
    Null,
}

impl Type {
    pub fn get_name(&self) -> String {
        match self {
            Type::Number => String::from("number"),
            Type::String => String::from("string"),
            Type::Char => String::from("char"),
            Type::Boolean => String::from("bool"),
            Type::Range => String::from("range"),
            Type::Fn => String::from("function"),
            Type::List => String::from("list"),
            Type::Custom => unimplemented!(),
            Type::Null => String::from("null"),
        }
    }
}

pub trait ForgeIter = Iterator<Item=Value> + fmt::Debug;

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(Rc<RefCell<String>>),
    Char(char),
    Boolean(bool),
    Range(f64, f64),
    Fn(Rc<String>, Rc<(Node<Args>, Node<Block>)>),
    List(Rc<RefCell<Vec<Value>>>),
    Map(Rc<RefCell<HashMap<Value, Value>>>),
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
            Value::List(l) => writeln!(f, "List({:?})", l.borrow()),
            Value::Map(m) => writeln!(f, "Map({:?})", m.borrow()),
            Value::Custom(c) => writeln!(f, "Custom({:?})", &c as *const _),
            Value::Null => writeln!(f, "Null"),
        }
    }
}

impl<V: Into<Value>, F: Fn() -> V + 'static> Obj for F {
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

/* TODO: Fix this
macro_rules! expand_args {
    ($params:expr, $caller:expr, $io:expr, $src:expr, 1) => ($caller.eval_expr(&$params.0[0].0, $io, $src)?);
    ($params:expr, $caller:expr, $io:expr, $src:expr, $n:expr, $left:tt) => (
        expand_args!($params, $caller, $io, $src, $left), $caller.eval_expr(&$params.0[$n].0, $io, $src)?
    );
}

macro_rules! impl_obj_for_fn {
    ($n:expr, $x:expr) => (
        impl<V: Into<Value> + 'static> Obj for fn(Value) -> V {
            fn eval_call(&self, params: &Node<Vec<Node<Expr>>>, caller: &mut dyn Scope, io: &mut dyn Io, src: &Rc<String>, _r_caller: SrcRef) -> ExecResult<Value> {
                if params.0.len() != $n {
                    Err(ExecError::At(params.1, Box::new(ExecError::WrongArgNum(
                        None, 0, params.0.len()
                    )))).map_err(|err| ExecError::WithSrc(src.clone(), Box::new(err)))
                } else {
                    Ok(self(expand_args!(params, caller, io, src, $n, $x)).into())
                }
            }
        }
    );
}
impl_obj_for_fn!(1);
impl_obj_for_fn!(1, 2);
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
            (Value::Map(x), Value::Map(y)) => Rc::ptr_eq(&x, &y),
            (Value::Null, Value::Null) => true,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match self {
            Value::Number(x) => x.to_bits().hash(state),
            Value::String(x) => x.borrow().as_str().hash(state),
            Value::Char(x) => x.hash(state),
            Value::Boolean(x) => x.hash(state),
            Value::Range(a, b) => {
                a.to_bits().hash(state);
                b.to_bits().hash(state);
            },
            Value::Fn(_, x) => Rc::into_raw(x.clone()).hash(state),
            Value::List(x) => Rc::into_raw(x.clone()).hash(state),
            Value::Map(x) => Rc::into_raw(x.clone()).hash(state),
            Value::Custom(x) => Rc::into_raw(x.clone()).hash(state),
            Value::Null => {},
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

    #[inline(always)]
    pub fn eval_call(&self, params: &Node<Vec<Node<Expr>>>, caller: &mut dyn Scope, io: &mut dyn Io, src: &Rc<String>, r_caller: SrcRef) -> ExecResult<Value> {
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

    #[inline(always)]
    pub fn get_type_name(&self) -> String {
        match self {
            Value::Number(_) => String::from("number"),
            Value::String(_) => String::from("string"),
            Value::Char(_) => String::from("char"),
            Value::Boolean(_) => String::from("bool"),
            Value::Range(_, _) => String::from("range"),
            Value::Fn(_, _) => String::from("function"),
            Value::List(_) => String::from("list"),
            Value::Map(_) => String::from("map"),
            Value::Custom(c) => c.get_type_name(),
            Value::Null => String::from("null"),
        }
    }

    #[inline(always)]
    pub fn get_display_text(&self) -> ExecResult<String> {
        Ok(match self {
            Value::Number(x) => format!("{}", x),
            Value::String(s) => s.borrow().clone(),
            Value::Char(c) => format!("{}", c),
            Value::Boolean(b) => format!("{}", b),
            Value::Range(x, y) => format!("{}..{}", x, y),
            Value::Fn(_, _) => String::from("<function>"),
            Value::List(l) => {
                let mut s = String::from("[");
                if let Some(i) = l.borrow().get(0) {
                    s += &i.get_display_text()?;
                }
                for item in l.borrow().get(1..).unwrap_or(&[]) {
                    s += ", ";
                    s += &item.get_display_text()?;
                }
                s.push(']');
                s
            },
            Value::Map(m) => {
                let mut s = String::from("[");
                for (i, (key, val)) in m.borrow().iter().enumerate() {
                    if i != 0 {
                        s += ", ";
                    }
                    s += &key.get_display_text()?;
                    s += ": ";
                    s += &val.get_display_text()?;
                }
                s.push(']');
                s
            },
            Value::Custom(c) => c.get_display_text()?,
            Value::Null => String::from("<null>"),
        })
    }

    #[inline(always)]
    pub fn eval_truth(&self, r: SrcRef) -> ExecResult<bool> {
        match self {
            Value::Boolean(b) => Ok(*b),
            Value::Custom(c) => c.eval_truth(r),
            _ => Err(ExecError::CannotDetermineTruthiness(r, self.get_type_name())),
        }
    }

    #[inline(always)]
    pub fn eval_index(&self, index: &Value, r: SrcRef) -> ExecResult<Value> {
        match (self, index) {
            (Value::String(s), Value::Number(i)) => Ok(s
                .borrow()
                .char_indices()
                .nth(*i as usize)
                .map(|(_, c)| Value::Char(c))
                .unwrap_or(Value::Null)
            ),
            (Value::String(s), Value::Range(a, b)) => Ok(Value::String(Rc::new(RefCell::new(s
                .borrow()
                .chars()
                .skip(*a as usize)
                .take(*b as usize - *a as usize)
                .collect()
            )))),
            (Value::List(l), Value::Number(i)) => Ok(l.borrow().get(*i as usize).cloned().unwrap_or(Value::Null)),
            (Value::List(l), Value::Range(x, y)) => Ok({
                if let Some(slice) = l.borrow().get(*x as usize..*y as usize) {
                    Value::List(Rc::new(RefCell::new(slice.iter().map(|v| v.clone()).collect())))
                } else {
                    Value::Null
                }
            }),
            (Value::Map(m), index) => Ok(m.borrow().get(index).cloned().unwrap_or(Value::Null)),
            (Value::Custom(c), index) => c.eval_index(index, r),
            (this, index) => Err(ExecError::CannotIndex(r, this.get_type_name(), index.get_type_name())),
        }
    }

    #[inline(always)]
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

    #[inline(always)]
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

    #[inline(always)]
    pub fn eval_clone(&self, refs: UnaryOpRef) -> ExecResult<Value> {
        match self {
            Value::Number(x) => Ok(Value::Number(*x)),
            Value::String(s) => Ok(Value::String(s.clone())),
            Value::Char(c) => Ok(Value::Char(*c)),
            Value::Boolean(b) => Ok(Value::Boolean(*b)),
            Value::Range(x, y) => Ok(Value::Range(*x, *y)),
            Value::Fn(s, f) => Ok(Value::Fn(s.clone(), f.clone())),
            Value::List(l) => Ok(Value::List(Rc::new(l.as_ref().clone()))),
            Value::Map(m) => Ok(Value::Map(Rc::new(m.as_ref().clone()))),
            Value::Custom(c) => c.eval_clone(refs),
            Value::Null => Ok(Value::Null),
        }
    }

    #[inline(always)]
    pub fn eval_mirror(&self, refs: UnaryOpRef) -> ExecResult<Value> {
        match self {
            Value::Number(x) => Ok(Value::Number(*x)),
            Value::String(s) => Ok(Value::String(s.clone())),
            Value::Char(c) => Ok(Value::Char(*c)),
            Value::Boolean(b) => Ok(Value::Boolean(*b)),
            Value::Range(x, y) => Ok(Value::Range(*x, *y)),
            Value::Fn(s, f) => Ok(Value::Fn(s.clone(), f.clone())),
            Value::List(l) => Ok(Value::List(Rc::new(RefCell::new(l.borrow().iter().map(|i| i.eval_mirror(refs)).collect::<Result<_, _>>()?)))),
            Value::Map(m) => Ok(Value::Map(Rc::new(RefCell::new(m.borrow().iter().map(|(k, v)| {
                Ok((k.eval_mirror(refs)?, v.eval_mirror(refs)?))
            }).collect::<Result<_, _>>()?)))),
            Value::Custom(c) => c.eval_mirror(refs),
            Value::Null => Ok(Value::Null),
        }
    }

    #[inline(always)]
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

    #[inline(always)]
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

    #[inline(always)]
    pub fn eval_rem(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(*x % *y)),
            (Value::Custom(c), rhs) => c.eval_rem(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "rem",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    #[inline(always)]
    pub fn eval_add(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x.clone() + y)),
            (Value::String(x), Value::String(y)) => Ok(Value::String(Rc::new(RefCell::new(x.borrow().clone() + &y.borrow())))),
            (Value::String(x), Value::Char(y)) => Ok(Value::String(Rc::new(RefCell::new(format!("{}{}", x.borrow(), y))))),
            (Value::String(x), Value::Number(y)) => Ok(Value::String(Rc::new(RefCell::new(x.borrow().clone() + &format!("{}", y))))),
            (Value::String(x), Value::Boolean(y)) => Ok(Value::String(Rc::new(RefCell::new(x.borrow().clone() + &format!("{}", y))))),
            (Value::String(x), Value::Null) => Ok(Value::String(Rc::new(RefCell::new(x.borrow().clone() + &"null")))),
            (Value::List(x), Value::List(y)) => {
                let mut v = x.borrow().clone();
                v.append(&mut y.borrow().clone());
                Ok(Value::List(Rc::new(RefCell::new(v))))
            },
            (Value::List(x), rhs) => {
                let mut v = x.borrow().clone();
                v.push(rhs.clone());
                Ok(Value::List(Rc::new(RefCell::new(v))))
            },
            (Value::Map(m), Value::List(l)) => if l.borrow().len() == 2 {
                let mut m = m.borrow().clone();
                m.insert(
                    l.borrow().get(0).unwrap().clone(),
                    l.borrow().get(1).unwrap().clone(),
                );
                Ok(Value::Map(Rc::new(RefCell::new(m))))
            } else {
                Err(ExecError::BinaryOp {
                    op: "insert",
                    left_type: self.get_type_name(),
                    right_type: format!("{} (length = {})", rhs.get_type_name(), l.borrow().len()),
                    refs,
                })
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

    #[inline(always)]
    pub fn eval_sub(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(*x - *y)),
            (Value::Map(m), rhs) => {
                let mut m = m.borrow().clone();
                let _ = m.remove(rhs);
                Ok(Value::Map(Rc::new(RefCell::new(m))))
            },
            (Value::Custom(c), rhs) => c.eval_sub(rhs, refs),
            (this, rhs) => Err(ExecError::BinaryOp {
                op: "sub",
                left_type: this.get_type_name(),
                right_type: rhs.get_type_name(),
                refs,
            }),
        }
    }

    #[inline(always)]
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

    #[inline(always)]
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

    #[inline(always)]
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

    #[inline(always)]
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

    #[inline(always)]
    pub fn eval_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x == *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x.borrow() == *y.borrow())),
            (Value::Char(x), Value::Char(y)) => Ok(Value::Boolean(*x == *y)),
            (Value::Boolean(x), Value::Boolean(y)) => Ok(Value::Boolean(*x == *y)),
            (Value::Fn(_, x), Value::Fn(_, y)) => Ok(Value::Boolean(Rc::ptr_eq(&x, &y))),
            (Value::List(x), Value::List(y)) => Ok(Value::Boolean(
                x.borrow().len() == y.borrow().len() &&
                x.borrow().iter().zip(y.borrow().iter()).all(|(x, y)| x.eq(y))
            )),
            (Value::Map(x), Value::Map(y)) => Ok(Value::Boolean(
                x.borrow().len() == y.borrow().len() &&
                x.borrow().iter().zip(y.borrow().iter()).all(|((xk, xv), (yk, yv))| xk.eq(yk) && yv.eq(yv))
            )),
            (Value::Custom(c), rhs) => c.eval_eq(rhs, refs),
            (Value::Null, Value::Null) => Ok(Value::Boolean(true)),
            _ => Ok(Value::Boolean(false)),
        }
    }

    #[inline(always)]
    pub fn eval_not_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, rhs) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(*x != *y)),
            (Value::String(x), Value::String(y)) => Ok(Value::Boolean(*x != *y)),
            (Value::Char(x), Value::Char(y)) => Ok(Value::Boolean(*x != *y)),
            (Value::Boolean(x), Value::Boolean(y)) => Ok(Value::Boolean(*x != *y)),
            (Value::Fn(_, x), Value::Fn(_, y)) => Ok(Value::Boolean(!Rc::ptr_eq(&x, &y))),
            (Value::List(x), Value::List(y)) => Ok(Value::Boolean(
                x.borrow().len() != y.borrow().len() ||
                !x.borrow().iter().zip(y.borrow().iter()).all(|(x, y)| x.eq(y))
            )),
            (Value::Map(x), Value::Map(y)) => Ok(Value::Boolean(
                x.borrow().len() != y.borrow().len() ||
                !x.borrow().iter().zip(y.borrow().iter()).all(|((xk, xv), (yk, yv))| xk.eq(yk) && yv.eq(yv))
            )),
            (Value::Custom(c), rhs) => c.eval_not_eq(rhs, refs),
            (Value::Null, Value::Null) => Ok(Value::Boolean(false)),
            _ => Ok(Value::Boolean(true)),
        }
    }

    #[inline(always)]
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

    #[inline(always)]
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

    #[inline(always)]
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

    #[inline(always)]
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

    #[inline(always)]
    pub fn eval_as(&self, ty: &Type, refs: BinaryOpRef) -> ExecResult<Value> {
        match (self, ty) {
            (Value::Number(x), Type::Char) => Ok(Value::Char(*x as u64 as u8 as char)),
            (Value::Number(s), Type::String) => Ok(Value::String(Rc::new(RefCell::new(format!("{}", s))))),
            (Value::Char(c), Type::Number) => Ok(Value::Number(*c as u8 as f64)),
            (Value::Char(c), Type::String) => Ok(Value::String(Rc::new(RefCell::new(format!("{}", c))))),
            (Value::Boolean(b), Type::String) => Ok(Value::String(Rc::new(RefCell::new(format!("{}", b))))),
            _ => Err(ExecError::BinaryOp {
                op: "as",
                left_type: self.get_type_name(),
                right_type: ty.get_name(),
                refs,
            }),
        }.map_err(|err| ExecError::At(refs.op, Box::new(err)))
    }

    #[inline(always)]
    pub fn eval_iter(&self, r: SrcRef) -> ExecResult<Box<ForgeIter>> {
        match self {
            Value::Range(x, y) => Ok(Box::new((*x as i64..*y as i64).map(|v| Value::Number(v as f64)))),
            Value::String(s) => Ok(Box::new(s.borrow().chars().collect::<Vec<_>>().into_iter().map(|c| Value::Char(c)))),
            Value::List(l) => Ok(Box::new(l.borrow().clone().into_iter())),
            Value::Custom(c) => c.eval_iter(r),
            _ => Err(ExecError::At(r, Box::new(ExecError::NotIterable(self.get_type_name())))),
        }
    }

    #[inline(always)]
    pub fn assign_index(&mut self, index: &Value, rhs: Value, r_idx: SrcRef, r_rhs: SrcRef) -> ExecResult<()> {
        let byte_indices = |s: &str, (a, b)| Ok((
            s
                .char_indices()
                .nth(a)
                .ok_or_else(|| ExecError::At(r_idx, Box::new(ExecError::InvalidIndex(self.get_type_name(), index.clone()))))?.0,
            s
                .char_indices()
                .chain(Some((s.len(), '_')))
                .nth(b as usize)
                .ok_or_else(|| ExecError::At(r_idx, Box::new(ExecError::InvalidIndex(self.get_type_name(), index.clone()))))?.0,
        ));
        match (&self, index, &rhs) {
            (Value::String(s), Value::Number(i), Value::Char(new_c)) => {
                let mut s = s.borrow_mut();
                let byte_idxs = byte_indices(&s, (*i as usize, *i as usize + 1))?;
                Ok(s.replace_range(byte_idxs.0..byte_idxs.1, &new_c.to_string())
            },
            (Value::String(_), Value::Number(_), rhs) => Err(ExecError::CannotIndexAssign(r_rhs, self.get_type_name(), rhs.get_type_name())),
            (Value::String(s), Value::Range(a, b), Value::String(new_s)) => {
                let mut s = s.borrow_mut();
                let byte_idxs = byte_indices(&s, (*a as usize, *b as usize))?;
                Ok(s.replace_range(byte_idxs.0..byte_idxs.1, &new_s.borrow())
            },
            (Value::String(_), Value::Range(_, _), rhs) => Err(ExecError::CannotIndexAssign(r_rhs, self.get_type_name(), rhs.get_type_name())),
            (Value::List(l), Value::Number(i), _) => {
                l
                    .borrow_mut()
                    .get_mut(*i as usize)
                    .map(|v| *v = rhs)
                    .ok_or_else(|| ExecError::At(r_idx, Box::new(ExecError::InvalidIndex(self.get_type_name(), index.clone()))))
            },
            (Value::List(l), Value::Range(a, b), Value::List(extra_l)) => {
                let extra_list = extra_l.borrow().clone();
                if *a as usize >= 0 && *b as usize <= l.borrow().len() {
                    let new_list = Value::List(Rc::new(RefCell::new(l
                        .borrow_mut()
                        .splice(*a as usize..*b as usize, extra_list)
                        .collect()
                    )));
                    *self = new_list;
                    Ok(())
                } else {
                    Err(ExecError::At(r_idx, Box::new(ExecError::InvalidIndex(self.get_type_name(), index.clone()))))
                }
            },
            (Value::Map(m), index, rhs) => {
                m.borrow_mut().insert(index.clone(), rhs.clone());
                Ok(())
            },
            (this, index, _) => Err(ExecError::CannotIndex(r_idx, this.get_type_name(), index.get_type_name())),
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
            Value::String(x) => x.borrow().eq(*other),
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
        Value::String(Rc::new(RefCell::new(other.to_string())))
    }
}

impl From<String> for Value {
    fn from(other: String) -> Self {
        Value::String(Rc::new(RefCell::new(other)))
    }
}

impl From<Range<i64>> for Value {
    fn from(other: Range<i64>) -> Self {
        Value::Range(other.start as f64, other.end as f64)
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(other: Vec<T>) -> Self {
        Value::List(Rc::new(RefCell::new(other.into_iter().map(|i| i.into()).collect())))
    }
}

impl<K: Into<Value> + Eq + Hash, V: Into<Value>> From<StdHashMap<K, V>> for Value {
    fn from(other: StdHashMap<K, V>) -> Self {
        Value::Map(Rc::new(RefCell::new(other.into_iter().map(|(k, v)| (k.into(), v.into())).collect())))
    }
}
