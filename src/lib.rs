#![feature(bind_by_move_pattern_guards)]

mod parser;
mod exec;
mod error;
mod output;

// Reexports
pub use exec::{
    ExecError,
    ExecResult,
    Io,
    DefaultIo,
    Value,
    Scope,
    Obj,
};
pub use error::{
    ForgeResult,
    ForgeError,
};

use std::{
    ops::DerefMut,
    collections::HashMap,
};

pub struct EngineBuilder {
    io: Box<dyn Io>,
}

impl EngineBuilder {
    pub fn with_io<T: Io + 'static>(mut self, io: T) -> Self {
        self.io = Box::new(io);
        self
    }

    pub fn finish(self) -> Engine {
        Engine {
            io: self.io,
            global_scope: GlobalScope::empty(),
        }
    }
}

pub struct Engine {
    io: Box<dyn Io>,
    global_scope: GlobalScope,
}

impl Engine {
    pub fn build() -> EngineBuilder {
        EngineBuilder {
            io: Box::new(DefaultIo),
        }
    }

    pub fn eval(&mut self, expr: &str) -> ForgeResult<Value> {
        let mut eval_fn = || {
            let expr = parser::Parser::new(expr)?.parse_expr()?;

            // TODO: Remove this
            //expr.print_debug(0);

            Ok(self.global_scope.eval_expr(&expr, self.io.deref_mut())?)
        };
        eval_fn().map_err(|e| ForgeError::InSrc(expr.to_string(), Box::new(e)))
    }

    pub fn exec(&mut self, module: &str) -> ForgeResult<()> {
        let mut exec_fn = || {
            let stmts = parser::Parser::new(module)?.parse_stmts()?;

            for stmt in &stmts {
                // stmt.0.print_debug(0); // TODO: Remove this
                self.global_scope.eval_stmt(&stmt.0, self.io.deref_mut())?;
            }

            Ok(())
        };
        exec_fn().map_err(|e| ForgeError::InSrc(module.to_string(), Box::new(e)))
    }

    pub fn prompt(&mut self, input: &str) -> ForgeResult<Option<Value>> {
        match parser::Parser::new(input)?.parse_stmts() {
            Ok(stmts) => {
                for stmt in &stmts {
                    self.global_scope.eval_stmt(&stmt.0, self.io.deref_mut()).map_err(|err|
                        ForgeError::InSrc(input.to_string(), Box::new(err.into()))
                    )?;
                }
                Ok(None)
            },
            Err(stmts_err) => Ok(Some(self.global_scope.eval_expr(
                &parser::Parser::new(input)?.parse_expr().map_err(|err|
                    ForgeError::InSrc(input.to_string(), Box::new(err.max(stmts_err).into()))
                )?,
                self.io.deref_mut(),
            ).map_err(|err| ForgeError::InSrc(input.to_string(), Box::new(err.into())))?)),
        }
    }
}

impl Default for Engine {
    fn default() -> Self {
        Engine::build().finish()
    }
}

struct GlobalScope {
    vars: HashMap<String, Value>,
}

impl GlobalScope {
    pub fn empty() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }
}

impl Scope for GlobalScope {
    fn get_var(&self, name: &str) -> ExecResult<Value> {
        self.vars
            .get(name)
            .cloned()
            .ok_or(ExecError::NoSuchItem(name.to_string()))
    }

    fn declare_var(&mut self, name: String, val: Value) -> ExecResult<()> {
        self.vars
            .insert(name.clone(), val)
            .and(Some(Err(ExecError::ItemExists(name))))
            .unwrap_or(Ok(()))
    }

    fn assign_var(&mut self, name: &str, val: Value) -> ExecResult<()> {
        self.vars
            .get_mut(name)
            .map(|v| *v = val)
            .ok_or(ExecError::NoSuchItem(name.to_string()))
    }

    fn as_scope_mut(&mut self) -> &mut dyn Scope {
        self
    }
}
