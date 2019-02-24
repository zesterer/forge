mod block_scope;
mod global_scope;
mod value;

// Reexports
pub use self::{
    value::Value,
    global_scope::GlobalScope,
};

use std::{
    fmt,
    io::{self, prelude::*},
    rc::Rc,
};
use crate::{
    output,
    parser::{
        SrcRef,
        ast::{
            Expr,
            Stmt,
            Block,
        },
    },
};
use block_scope::BlockScope;

#[derive(Debug)]
pub enum ExecError {
    CannotCall(String),
    WrongArgNum(SrcRef, usize, usize),
    CannotDisplay(String),
    CouldNotParse(String),
    Io(io::Error),
    Truthiness(SrcRef, String),
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
            ExecError::WrongArgNum(r_args, x, y) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, *r_args, psrc, depth + 1))
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Tried to call a function with the wrong number of parameters. Expected {}, found {}.", output::Repeat(' ', (depth + 1) * 3), x, y))
            },
            ExecError::CannotCall(s) => {
                Ok(())
                    .and_then(|_| output::fmt_ref(f, r, src, depth + 1))
                    .and_then(|_| writeln!(f, "{}Cannot call value of type '{}'.", output::Repeat(' ', (depth + 1) * 3), s))
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
            _ => Ok(()),
        }
    }

    pub fn fmt_nice(&self, f: &mut fmt::Formatter, src: Option<&str>, psrc: Option<&str>, depth: usize) -> fmt::Result {
        match self {
            ExecError::Truthiness(r, expr_type) => {
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

pub trait Obj: fmt::Debug {
    fn get_type_name(&self) -> String;

    fn get_display_text(&self) -> ExecResult<String> {
        Err(ExecError::CannotDisplay(self.get_type_name()))
    }

    fn eval_truth(&self, r: SrcRef) -> ExecResult<bool> {
        Err(ExecError::Truthiness(r, self.get_type_name()))
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

    fn eval_mul(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "mul",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_div(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "div",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_mod(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "mod",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_add(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "add",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_sub(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "sub",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_greater(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "greater",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_greater_eq(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "greater_eq",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_less(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "less",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_less_eq(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "less_eq",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_eq(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "eq",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_not_eq(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "not_eq",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_and(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "and",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_or(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "or",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }

    fn eval_xor(&self, _rhs: &Value, refs: BinaryOpRef) -> ExecResult<Value> {
        Err(ExecError::BinaryOp {
            op: "xor",
            left_type: self.get_type_name(),
            right_type: self.get_type_name(),
            refs,
        })
    }
}

pub trait Scope {
    fn get_var(&self, name: &str) -> ExecResult<Value>;
    fn declare_var(&mut self, name: String, val: Value) -> ExecResult<()>;
    fn assign_var(&mut self, name: &str, val: Value) -> ExecResult<()>;
    fn as_scope_mut(&mut self) -> &mut dyn Scope;

    fn eval_expr(&self, expr: &Expr, io: &mut dyn Io, src: &Rc<String>) -> ExecResult<Value> {
        let src_map = |err| ExecError::WithSrc(src.clone(), Box::new(err));

        match expr {
            Expr::None => Ok(Value::Null),
            Expr::LiteralNumber(x) => Ok(Value::Number(*x)),
            Expr::LiteralString(s) => Ok(Value::String(s.to_string())),
            Expr::LiteralBoolean(b) => Ok(Value::Boolean(*b)),
            Expr::LiteralNull => Ok(Value::Null),
            Expr::Ident(name) =>
                self.get_var(&name.0).map_err(|err| ExecError::At(name.1, Box::new(err))).map_err(src_map),
            Expr::DotAccess(_, _, _) => unimplemented!(),
            Expr::Call(_r, expr, params) => {
                match self.eval_expr(&expr.0, io, src)? {
                    Value::Fn(code, f) => if ((f.0).0).0.len() != params.0.len() {
                        return Err(ExecError::WithPrevSrc(code, Box::new(ExecError::At(params.1, Box::new(ExecError::WrongArgNum(
                            (f.0).1, ((f.0).0).0.len(), params.0.len()
                        )))))).map_err(src_map);
                    } else {
                        // TODO: Properly scope functions
                        let mut scope = GlobalScope::empty();
                        for (arg, param) in ((f.0).0).0.iter().zip(&params.0) {
                            scope.declare_var(arg.0.clone(), self.eval_expr(&param.0, io, src)?).map_err(src_map)?;
                        }
                        Ok(scope.eval_block(&(f.1).0, io, &code)?.unwrap_or(Value::Null))
                    },
                    val => Err(ExecError::At(expr.1, Box::new(ExecError::CannotCall(val.get_type_name())))),
                }
            },
            Expr::UnaryNot(r, expr) =>
                self.eval_expr(&expr.0, io, src)?.eval_not(UnaryOpRef { op: *r, expr: expr.1 }).map_err(src_map),
            Expr::UnaryNeg(r, expr) =>
                self.eval_expr(&expr.0, io, src)?.eval_neg(UnaryOpRef { op: *r, expr: expr.1 }).map_err(src_map),
            Expr::UnaryInput(r, expr) => {
                let text = self.eval_expr(&expr.0, io, src)?.get_display_text()
                    .map_err(|err| ExecError::At(expr.1, Box::new(err)))
                    .map_err(src_map)?;
                let input = io.input(text)
                    .map_err(|err| ExecError::At(r.union(&expr.1), Box::new(err)))
                    .map_err(src_map)?;
                input
                    .trim().parse().map(|n| Value::Number(n))
                    .or_else(|_| input.trim().parse().map(|n| Value::Boolean(n)))
                    .or_else(|_| if input.trim() == "null" { Ok(Value::Null) } else { Err(()) })
                    .or_else(|_| input.parse().map(|n| Value::String(n)))
                    .map_err(|_| ExecError::At(*r, Box::new(ExecError::CouldNotParse(input))))
                    .map_err(src_map)
            },
            Expr::BinaryMul(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_mul(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryDiv(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_div(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
            Expr::BinaryMod(r, left, right) =>
                self.eval_expr(&left.0, io, src)?.eval_mod(&self.eval_expr(&right.0, io, src).map_err(src_map)?, BinaryOpRef { op: *r, left: left.1, right: right.1 }),
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
            Stmt::Decl(ident, expr) => {
                let val = self.eval_expr(&expr.0, io, src)?;
                self.declare_var(ident.0.clone(), val)
                    .map_err(|err| ExecError::At(ident.1, Box::new(err)))?;
                Ok(None)
            },
            Stmt::Assign(ident, expr) => {
                let val = self.eval_expr(&expr.0, io, src)?;
                self.assign_var(&ident.0, val)
                    .map_err(|err| ExecError::At(ident.1, Box::new(err)))?;
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
