use hashbrown::HashMap;
use super::{
    ExecError,
    ExecResult,
    Scope,
    Value,
};

pub struct BlockScope<'a> {
    vars: HashMap<String, Value>,
    parent: &'a mut dyn Scope,
}

impl<'a> BlockScope<'a> {
    pub fn new(parent: &'a mut dyn Scope) -> Self {
        Self {
            vars: HashMap::new(),
            parent,
        }
    }
}

impl<'a> Scope for BlockScope<'a> {
    fn get_var(&self, name: &str) -> ExecResult<Value> {
        self.vars
            .get(name)
            .cloned()
            .ok_or(ExecError::NoSuchItem(name.to_string()))
            .or_else(|_| self.parent.get_var(name))
    }

    fn declare_var(&mut self, name: String, val: Value) -> ExecResult<()> {
        self.vars.insert(name, val);
        Ok(())
    }

    fn assign_var(&mut self, name: &str, val: Value) -> ExecResult<()> {
        self.vars
            .get_mut(name)
            .map(|v| *v = val.clone())
            .ok_or(ExecError::NoSuchItem(name.to_string()))
            .or_else(|_| self.parent.assign_var(name, val))
    }

    fn as_scope_mut(&mut self) -> &mut dyn Scope {
        self
    }
}
