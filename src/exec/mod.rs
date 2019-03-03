mod block_scope;
mod global_scope;
mod value;

// Reexports
pub use self::{
    value::{
        Value,
        Type,
        ForgeIter,
    },
    global_scope::GlobalScope,
};

use std::{
    fmt,
    io::{self, prelude::*},
    rc::Rc,
    any::Any,
    cell::RefCell,
};
use hashbrown::HashMap;
use crate::{
    output,
    parser::{
        SrcRef,
        ast::{
            Expr,
            LVal,
            Stmt,
            Block,
            Node,
        },
    },
};
use block_scope::BlockScope;

#[derive(Debug)]
pub enum ExecError {
    NotIterator,
    NotAType,
    InvalidIndex(String, Value),
    NotNumeric(String),
    NotIterable(String),
    CannotCall(String),
    CannotIndex(SrcRef, String, String),
    CannotIndexAssign(SrcRef, String, String),
    WrongArgNum(Option<SrcRef>, usize, usize),
    CannotDisplay(String),
    CouldNotParse(String),
    Io(io::Error),
    CannotDetermineTruthiness(SrcRef, String),
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
    NoSuchItem(String),
    ItemExists(String),
    At(SrcRef, Box<ExecError>),
    WithSrc(Rc<String>, Box<ExecError>),
    WithPrevSrc(Rc<String>, Box<ExecError>),
}

impl ExecError {
    pub fn fmt_nice_located(&self, f: &mut fmt::Formatter, src: Option<&str>, psrc: Option<&str>, depth: usize, r: SrcRef) -> fmt::Result {
        writeln!(f, "[ERROR] Runtime error at {}...", r.start())?;
        match self {
            ExecError::NotAType => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Expression is not a type.", output::Repeat(' ', (depth + 1) * 3)))
            },
            ExecError::InvalidIndex(ty, val) => {
                let val = val.get_display_text().unwrap_or("<cannot display value>".to_string());
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Invalid index '{}' used to index value of type '{}'.", output::Repeat(' ', (depth + 1) * 3), val, ty))
            },
            ExecError::NotIterator => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Yielded value is not an iterator.", output::Repeat(' ', (depth + 1) * 3)))
            },
            ExecError::NotNumeric(s) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Value of type '{}' is not numeric.", output::Repeat(' ', (depth + 1) * 3), s))
            },
            ExecError::NotIterable(s) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Value of type '{}' is not iterable.", output::Repeat(' ', (depth + 1) * 3), s))
            },
            ExecError::CannotIndex(r_index, ty, ty_index) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| output::fmt_ref(f, *r_index, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Cannot index value of type '{}' with value of type '{}'.", output::Repeat(' ', (depth + 1) * 3), ty, ty_index))
            },
            ExecError::CannotIndexAssign(r_index, ty, ty_rvalue) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| output::fmt_ref(f, *r_index, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Cannot assign index of value of type '{}' as value of type '{}'.", output::Repeat(' ', (depth + 1) * 3), ty, ty_rvalue))
            },
            ExecError::CannotCall(s) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Cannot call value of type '{}'.", output::Repeat(' ', (depth + 1) * 3), s))
            },
            ExecError::WrongArgNum(r_args, x, y) => {
                if let Some(r_args) = r_args {
                    output::fmt_ref(f, *r_args, psrc, depth + 1)?;
                }
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Tried to call a function with the wrong number of parameters. Expected {}, found {}.", output::Repeat(' ', (depth + 1) * 3), x, y))
            },
            ExecError::CannotDisplay(s) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Cannot display value of type '{}'.", output::Repeat(' ', (depth + 1) * 3), s))
            },
            ExecError::CouldNotParse(s) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Could not parse '{}' into a value.", output::Repeat(' ', (depth + 1) * 3), s))
            },
            ExecError::Io(io) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}I/O error: {}.", output::Repeat(' ', (depth + 1) * 3), io))
            },
            ExecError::NoSuchItem(item) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Cannot find item '{}' within the current scope.", output::Repeat(' ', (depth + 1) * 3), item))
            },
            ExecError::ItemExists(item) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Item '{}' already exist in the current scope.", output::Repeat(' ', (depth + 1) * 3), item))
            },
            ExecError::WithSrc(src, err) => err.fmt_nice_located(f, Some(&src), psrc, depth, r),
            ExecError::WithPrevSrc(psrc, err) => err.fmt_nice_located(f, src, Some(&psrc), depth, r),
            ExecError::At(r, err) => err.fmt_nice_located(f, src, psrc, depth, *r),
            _ => Ok(()),
        }
    }

    pub fn fmt_nice(&self, f: &mut fmt::Formatter, src: Option<&str>, psrc: Option<&str>, depth: usize) -> fmt::Result {
        match self {
            ExecError::CannotDetermineTruthiness(r, expr_type) => {
                Ok(())
                    .and_then(|_| writeln!(f, "[ERROR] Runtime error at {}...", r.start()))
                    .and_then(|_| output::fmt_ref(f, *r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Cannot determine the truthiness of value of type '{}'. Did you mean for this to be a bool?", output::Repeat(' ', (depth + 1) * 3), expr_type))
            },
            ExecError::UnaryOp { op, expr_type, refs } => {
                Ok(())
                    .and_then(|_| writeln!(f, "[ERROR] Runtime error at {}...", refs.op.start()))
                    .and_then(|_| output::fmt_ref(f, refs.op, src, depth + 1))
                    .and_then(|_| output::fmt_ref(f, refs.expr, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Cannot apply unary operator '{}' to value of type '{}'.", output::Repeat(' ', (depth + 1) * 3), op, expr_type))
            },
            ExecError::BinaryOp { op, left_type, right_type, refs } => {
                Ok(())
                    .and_then(|_| writeln!(f, "[ERROR] Runtime error at {}...", refs.op.start()))
                    .and_then(|_| output::fmt_ref(f, refs.left, src, depth + 1))
                    .and_then(|_| output::fmt_ref(f, refs.right, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Cannot apply binary operator '{}' to values of types '{}' and '{}'.", output::Repeat(' ', (depth + 1) * 3), op, left_type, right_type))
            },
            ExecError::At(r, err) => err.fmt_nice_located(f, src, psrc, depth, *r),
            ExecError::WithSrc(src, err) => err.fmt_nice(f, Some(&src), psrc, depth),
            ExecError::WithPrevSrc(psrc, err) => err.fmt_nice(f, src, Some(&psrc), depth),
            ExecError::Io(_) => Ok(()),
            ExecError::NotAType => Ok(()),
            ExecError::InvalidIndex(_, _) => Ok(()),
            ExecError::NotIterator => Ok(()),
            ExecError::NotNumeric(_) => Ok(()),
            ExecError::NotIterable(_) => Ok(()),
            ExecError::CannotIndex(_, _, _) => Ok(()),
            ExecError::CannotIndexAssign(_, _, _) => Ok(()),
            ExecError::CannotCall(_) => Ok(()),
            ExecError::WrongArgNum(_, _, _) => Ok(()),
            ExecError::CannotDisplay(_) => Ok(()),
            ExecError::CouldNotParse(_) => Ok(()),
            ExecError::NoSuchItem(_) => Ok(()),
            ExecError::ItemExists(_) => Ok(()),
        }
    }
}

pub type ExecResult<T> = Result<T, ExecError>;

pub trait Io {
    fn input(&mut self, s: String) -> ExecResult<String>;
    fn print(&mut self, s: String) -> ExecResult<()>;
}

pub struct DefaultIo;

impl Io for DefaultIo {
    fn input(&mut self, s: String) -> ExecResult<String> {
        print!("{}", s);
        io::stdout().flush()
            .map_err(|err| ExecError::Io(err))?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)
            .map_err(|err| ExecError::Io(err))?;
        Ok(input.replace('\n', ""))
    }

    fn print(&mut self, s: String) -> ExecResult<()> {
        println!("{}", s);
        Ok(())
    }
}

#[derive(Copy, Clone, Debug)]
pub struct UnaryOpRef {
    op: SrcRef,
    expr: SrcRef,
}

#[derive(Copy, Clone, Debug)]
pub struct BinaryOpRef {
    op: SrcRef,
    left: SrcRef,
    right: SrcRef,
}

pub trait Obj: 'static {
    fn get_type_name(&self) -> String {
        format!("{:?}", self.type_id())
    }

    fn get_display_text(&self) -> ExecResult<String> {
        Err(ExecError::CannotDisplay(self.get_type_name()))
    }

    fn eval_call(&self, _params: &Node<Vec<Node<Expr>>>, _caller: &mut dyn Scope, _io: &mut dyn Io, _src: &Rc<String>, r_caller: SrcRef) -> ExecResult<Value> {
        Err(ExecError::At(r_caller, Box::new(ExecError::CannotCall(self.get_type_name()))))
    }

    fn eval_truth(&self, r: SrcRef) -> ExecResult<bool> {
        Err(ExecError::CannotDetermineTruthiness(r, self.get_type_name()))
    }

    fn eval_index(&self, index: &Value, r: SrcRef) -> ExecResult<Value> {
        Err(ExecError::CannotIndex(r, self.get_type_name(), index.get_type_name()))
    }

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

    fn eval_clone(&self, refs: UnaryOpRef) -> ExecResult<Value> {
        Err(ExecError::UnaryOp {
            op: "clone",
            expr_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_mirror(&self, refs: UnaryOpRef) -> ExecResult<Value> {
        Err(ExecError::UnaryOp {
            op: "mirror",
            expr_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_mul(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "mul",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_div(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "div",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_rem(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "rem",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_add(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "add",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_sub(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "sub",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_greater(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "greater",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_greater_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "greater_eq",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_less(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "less",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_less_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "less_eq",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "eq",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_not_eq(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "not_eq",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_and(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "and",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_or(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "or",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_xor(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "xor",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_range(&self, rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "range",
            left_type: self.get_type_name(),
            right_type: rhs.get_type_name(),
            refs,
        })
    }

    fn eval_as(&self, ty: &Type, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "as",
            left_type: self.get_type_name(),
            right_type: ty.get_name(),
            refs,
        })
    }

    fn eval_iter(&self, r: SrcRef) -> ExecResult<Box<ForgeIter>> {
        Err(ExecError::At(r, Box::new(ExecError::NotIterable(self.get_type_name()))))
    }

    fn assign_index(&self, index: &Value, rhs: Value, r_idx: SrcRef, r_rhs: SrcRef) -> ExecResult<()> {
        Err(ExecError::CannotIndex(r_idx, self.get_type_name(), index.get_type_name()))
    }
}

pub trait Scope {
    fn get_var(&self, name: &str) -> ExecResult<Value>;
    fn take_var(&mut self, name: &str) -> Option<Value>;
    fn declare_var(&mut self, name: String, val: Value);
    fn assign_var(&mut self, name: &str, val: Value) -> ExecResult<()>;
    fn list(&self);
    fn as_scope_mut(&mut self) -> &mut dyn Scope;

    fn eval_type(&mut self, expr: &Expr, io: &mut dyn Io, src: &Rc<String>, r: SrcRef) -> ExecResult<Type> {
        let src_map = |err| ExecError::WithSrc(src.clone(), Box::new(err));

        match expr {
            Expr::Ident(name) => match name.0.as_str() {
                "num" => Ok(Type::Number),
                "str" => Ok(Type::String),
                "char" => Ok(Type::Char),
                "bool" => Ok(Type::Boolean),
                "range" => Ok(Type::Range),
                "fn" => Ok(Type::Fn),
                "list" => Ok(Type::List),
                "Custom" => Ok(Type::Custom),
                "null" => Ok(Type::Null),
                name => Err(ExecError::NotAType)
                    .map_err(|err| ExecError::At(r, Box::new(err)))
                    .map_err(src_map),
            },
            _ => Err(ExecError::NotAType)
                .map_err(|err| ExecError::At(r, Box::new(err)))
                .map_err(src_map),
        }
    }

    fn eval_expr(&mut self, expr: &Expr, io: &mut dyn Io, src: &Rc<String>) -> ExecResult<Value> {
        let src_map = |err| ExecError::WithSrc(src.clone(), Box::new(err));

        match expr {
            Expr::None => Ok(Value::Null),
            Expr::LiteralNumber(x) => Ok(Value::Number(*x)),
            Expr::LiteralString(s) => Ok(Value::String(Rc::new(RefCell::new(s.to_string())))),
            Expr::LiteralChar(c) => Ok(Value::Char(*c)),
            Expr::LiteralBoolean(b) => Ok(Value::Boolean(*b)),
            Expr::LiteralNull => Ok(Value::Null),
            Expr::Ident(name) =>
                self.get_var(&name.0)
                    .map_err(|err| ExecError::At(name.1, Box::new(err)))
                    .map_err(src_map),
            Expr::DotAccess(_, _, _) => unimplemented!(),
            Expr::Index(_r, expr, index) => {
                self.eval_expr(&expr.0, io, src)
                    .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                    .map_err(src_map)?
                    .eval_index(
                        &self.eval_expr(&index.0, io, src)
                            .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                            .map_err(src_map)?,
                        index.1,
                    )
                    .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                    .map_err(src_map)
            },
            Expr::Call(_r, expr, params) => {
                self.eval_expr(&expr.0, io, src)
                    .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                    .map_err(src_map)?
                    .eval_call(params, self.as_scope_mut(), io, src, expr.1)
            },
            Expr::List(items) => {
                let mut list_items = vec![];
                for item in &items.0 {
                    list_items.push(
                        self.eval_expr(&item.0, io, src)
                            .map_err(|err| ExecError::At(item.1, Box::new(err)))
                            .map_err(src_map)?,
                    );
                }
                Ok(Value::List(Rc::new(RefCell::new(list_items))))
            },
            Expr::ListClone(item, num) => {
                match self.eval_expr(&num.0, io, src)
                    .map_err(|err| ExecError::At(num.1, Box::new(err)))
                    .map_err(src_map)?
                {
                    Value::Number(x) => {
                        let mut list_items = Vec::with_capacity(x as usize);
                        let item_val = self.eval_expr(&item.0, io, src)
                            .map_err(|err| ExecError::At(item.1, Box::new(err)))
                            .map_err(src_map)?;

                        for _ in 0..x as usize {
                            list_items.push(
                                item_val.eval_clone(UnaryOpRef { op: item.1.union(&num.1), expr: item.1 })
                                    .map_err(|err| ExecError::At(item.1, Box::new(err)))
                                    .map_err(src_map)?
                            );
                        }

                        Ok(Value::List(Rc::new(RefCell::new(list_items))))
                    },
                    val => Err(ExecError::NotNumeric(val.get_type_name()))
                        .map_err(|err| ExecError::At(num.1, Box::new(err)))
                        .map_err(src_map),
                }
            },
            Expr::Map(maps) => {
                let mut hmap = HashMap::new();
                for (key, val) in &maps.0 {
                    hmap.insert(
                        self.eval_expr(&key.0, io, src)
                            .map_err(|err| ExecError::At(key.1, Box::new(err)))
                            .map_err(src_map)?,
                        self.eval_expr(&val.0, io, src)
                            .map_err(|err| ExecError::At(val.1, Box::new(err)))
                            .map_err(src_map)?,
                    );
                }
                Ok(Value::Map(Rc::new(RefCell::new(hmap))))
            },

            Expr::UnaryNot(r, expr) =>
                self.eval_expr(&expr.0, io, src)?.eval_not(UnaryOpRef { op: *r, expr: expr.1 }).map_err(src_map),
            Expr::UnaryNeg(r, expr) =>
                self.eval_expr(&expr.0, io, src)?.eval_neg(UnaryOpRef { op: *r, expr: expr.1 }).map_err(src_map),
            Expr::UnaryInput(r, expr) => {
                let text = self.eval_expr(&expr.0, io, src)
                    .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                    .map_err(src_map)?
                    .get_display_text()
                    .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                    .map_err(src_map)?;
                let input = io.input(text)
                    .map_err(|err| ExecError::At(r.union(&expr.1), Box::new(err)))
                    .map_err(src_map)?;
                input
                    .trim().parse().map(|n| Value::Number(n))
                    .or_else(|_| input.trim().parse().map(|n| Value::Boolean(n)))
                    .or_else(|_| if input.trim() == "null" { Ok(Value::Null) } else { Err(()) })
                    .or_else(|_| input.parse().map(|n| Value::String(Rc::new(RefCell::new(n)))))
                    .map_err(|_| ExecError::At(*r, Box::new(ExecError::CouldNotParse(input))))
                    .map_err(src_map)
            },
            Expr::UnaryClone(r, expr) =>
                self.eval_expr(&expr.0, io, src)?.eval_clone(UnaryOpRef { op: *r, expr: expr.1 }).map_err(src_map),
            Expr::UnaryMirror(r, expr) =>
                self.eval_expr(&expr.0, io, src)?.eval_mirror(UnaryOpRef { op: *r, expr: expr.1 }).map_err(src_map),

            Expr::BinaryMul(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_mul(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryDiv(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_div(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryRem(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_rem(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryAdd(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_add(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinarySub(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_sub(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryGreater(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_greater(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryGreaterEq(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_greater_eq(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryLess(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_less(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryLessEq(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_less_eq(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryEq(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_eq(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryNotEq(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_not_eq(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryAnd(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_and(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryOr(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_or(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryXor(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_xor(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryRange(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_range(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryAs(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_as(&self.eval_type(&right.0, io, src, right.1).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryAssign(r, lvalue, rvalue) => {
                let val = self.eval_expr(&rvalue.0, io, src)
                    .map_err(|err| ExecError::At(rvalue.1, Box::new(err)))
                    .map_err(src_map)?;

                match &lvalue.0 {
                    LVal::Local(ident) => {
                        self.assign_var(&ident.0, val)
                            .map_err(|err| ExecError::At(ident.1, Box::new(err)))
                            .map_err(src_map)?;
                        Ok(Value::Null)
                    },
                    LVal::Index(expr, index) => {
                        let mut container = self.eval_expr(&expr.0, io, src)
                            .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                            .map_err(src_map)?;
                        let index_val = self.eval_expr(&index.0, io, src)
                            .map_err(|err| ExecError::At(index.1, Box::new(err)))
                            .map_err(src_map)?;
                        container.assign_index(&index_val, val, index.1, rvalue.1)
                            .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                            .map_err(src_map)?;
                        Ok(Value::Null)
                    },
                }
            },
            Expr::BinaryAddAssign(r, lvalue, expr) => {
                let factor = self.eval_expr(&expr.0, io, src)
                    .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                    .map_err(src_map)?;

                match &lvalue.0 {
                    LVal::Local(ident) => {
                        let prev = self.get_var(&ident.0).map_err(|err| ExecError::At(ident.1, Box::new(err))).map_err(src_map)?;
                        self.assign_var(&ident.0, prev.eval_add(&factor, BinaryOpRef { op: *r, left: lvalue.1, right: expr.1 }).map_err(src_map)?)
                            .map_err(|err| ExecError::At(ident.1, Box::new(err)))
                            .map_err(src_map)?;
                        Ok(Value::Null)
                    },
                    LVal::Index(_, _) => unimplemented!(),
                }
            },
            Expr::BinarySubAssign(r, lvalue, expr) => {
                let factor = self.eval_expr(&expr.0, io, src)
                    .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                    .map_err(src_map)?;

                match &lvalue.0 {
                    LVal::Local(ident) => {
                        let prev = self.get_var(&ident.0).map_err(|err| ExecError::At(ident.1, Box::new(err))).map_err(src_map)?;
                        self.assign_var(&ident.0, prev.eval_sub(&factor, BinaryOpRef { op: *r, left: lvalue.1, right: expr.1 }).map_err(src_map)?)
                            .map_err(|err| ExecError::At(ident.1, Box::new(err)))
                            .map_err(src_map)?;
                        Ok(Value::Null)
                    },
                    LVal::Index(_, _) => unimplemented!(),
                }
            },
            Expr::BinaryMulAssign(r, lvalue, expr) => {
                let factor = self.eval_expr(&expr.0, io, src)
                    .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                    .map_err(src_map)?;

                match &lvalue.0 {
                    LVal::Local(ident) => {
                        let prev = self.get_var(&ident.0).map_err(|err| ExecError::At(ident.1, Box::new(err))).map_err(src_map)?;
                        self.assign_var(&ident.0, prev.eval_mul(&factor, BinaryOpRef { op: *r, left: lvalue.1, right: expr.1 }).map_err(src_map)?)
                            .map_err(|err| ExecError::At(ident.1, Box::new(err)))
                            .map_err(src_map)?;
                        Ok(Value::Null)
                    },
                    LVal::Index(_, _) => unimplemented!(),
                }
            },
            Expr::BinaryDivAssign(r, lvalue, expr) => {
                let factor = self.eval_expr(&expr.0, io, src)
                    .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                    .map_err(src_map)?;

                match &lvalue.0 {
                    LVal::Local(ident) => {
                        let prev = self.get_var(&ident.0).map_err(|err| ExecError::At(ident.1, Box::new(err))).map_err(src_map)?;
                        self.assign_var(&ident.0, prev.eval_div(&factor, BinaryOpRef { op: *r, left: lvalue.1, right: expr.1 }).map_err(src_map)?)
                            .map_err(|err| ExecError::At(ident.1, Box::new(err)))
                            .map_err(src_map)?;
                        Ok(Value::Null)
                    },
                    LVal::Index(_, _) => unimplemented!(),
                }
            },
            Expr::BinaryRemAssign(r, lvalue, expr) => {
                let factor = self.eval_expr(&expr.0, io, src)
                    .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                    .map_err(src_map)?;

                match &lvalue.0 {
                    LVal::Local(ident) => {
                        let prev = self.get_var(&ident.0).map_err(|err| ExecError::At(ident.1, Box::new(err))).map_err(src_map)?;
                        self.assign_var(&ident.0, prev.eval_rem(&factor, BinaryOpRef { op: *r, left: lvalue.1, right: expr.1 }).map_err(src_map)?)
                            .map_err(|err| ExecError::At(ident.1, Box::new(err)))
                            .map_err(src_map)?;
                        Ok(Value::Null)
                    },
                    LVal::Index(_, _) => unimplemented!(),
                }
            },
            Expr::Fn(code, rc) =>
                Ok(Value::Fn(code.clone(), rc.clone()))
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt, io: &mut dyn Io, src: &Rc<String>) -> ExecResult<Option<Value>> {
        match stmt {
            Stmt::Expr(expr) => { self.eval_expr(&expr.0, io, src)?; Ok(None) },
            Stmt::Print(expr) => {
                let text = self.eval_expr(&expr.0, io, src)?.get_display_text()
                    .map_err(|err| ExecError::At(expr.1, Box::new(err)))?;
                io.print(text).map(|_| None)
            },
            Stmt::If(expr, block) => {
                if self.eval_expr(&expr.0, io, src)?.eval_truth(expr.1)? {
                    if let Some(val) = BlockScope::new(self.as_scope_mut()).eval_block(&block.0, io, src)? {
                        return Ok(Some(val));
                    }
                }
                Ok(None)
            },
            Stmt::IfElse(expr, true_block, false_block) => {
                if self.eval_expr(&expr.0, io, src)?.eval_truth(expr.1)? {
                    if let Some(val) = BlockScope::new(self.as_scope_mut()).eval_block(&true_block.0, io, src)? {
                        return Ok(Some(val));
                    }
                } else {
                    if let Some(val) = BlockScope::new(self.as_scope_mut()).eval_block(&false_block.0, io, src)? {
                        return Ok(Some(val));
                    }
                }
                Ok(None)
            },
            Stmt::While(expr, block) => {
                while self.eval_expr(&expr.0, io, src)?.eval_truth(expr.1)? {
                    if let Some(val) = BlockScope::new(self.as_scope_mut()).eval_block(&block.0, io, src)? {
                        return Ok(Some(val));
                    }
                }
                Ok(None)
            },
            Stmt::For(ident, expr, block) => {
                let iter = self.eval_expr(&expr.0, io, src)?.eval_iter(expr.1)?;
                for item in iter {
                    let mut scope = BlockScope::new(self.as_scope_mut());
                    scope.declare_var(ident.0.clone(), item);
                    if let Some(val) = scope.eval_block(&block.0, io, src)? {
                        return Ok(Some(val));
                    }
                }
                Ok(None)
            },
            Stmt::Decl(ident, expr) => {
                let val = self.eval_expr(&expr.0, io, src)?;
                self.declare_var(ident.0.clone(), val);
                Ok(None)
            },
            Stmt::Return(expr) => {
                let val = self.eval_expr(&expr.0, io, src)
                    .map_err(|err| ExecError::At(expr.1, Box::new(err)))?;
                Ok(Some(val))
            },
        }
    }

    fn eval_block(&mut self, block: &Block, io: &mut dyn Io, src: &Rc<String>) -> ExecResult<Option<Value>> {
        for stmt in &block.0 {
            if let Some(val) = self.eval_stmt(&stmt.0, io, src)? {
                return Ok(Some(val));
            }
        }
        Ok(None)
    }
}
