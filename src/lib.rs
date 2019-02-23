#![feature(bind_by_move_pattern_guards)]

mod parser;
mod exec;
mod error;
mod output;

// Reexports
pub use exec::{
    Io,
    DefaultIo,
    Value,
    Scope,
};
pub use error::{
    ForgeResult,
    ForgeError,
};

use std::ops::DerefMut;

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
            global_scope: GlobalScope,
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

    pub fn execute(&mut self, module: &str) -> ForgeResult<()> {
        let mut execute_fn = || {
            let stmts = parser::Parser::new(module)?.parse_stmts()?;

            for stmt in &stmts {
                // stmt.0.print_debug(0); // TODO: Remove this
                self.global_scope.eval_stmt(&stmt.0, self.io.deref_mut())?;
            }

            Ok(())
        };
        execute_fn().map_err(|e| ForgeError::InSrc(module.to_string(), Box::new(e)))
    }
}

impl Default for Engine {
    fn default() -> Self {
        Engine::build().finish()
    }
}

struct GlobalScope;

impl Scope for GlobalScope {}
