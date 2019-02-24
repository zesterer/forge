use hashbrown::HashMap;
use super::{
    ExecError,
    ExecResult,
    Scope,
    Value,
};

pub struct GlobalScope {
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
