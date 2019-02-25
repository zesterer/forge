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

    fn take_var(&mut self, name: &str) -> Option<Value> {
        self.vars
            .remove(name)
    }

    fn declare_var(&mut self, name: String, val: Value) {
        self.vars.insert(name, val);
    }

    fn assign_var(&mut self, name: &str, val: Value) -> ExecResult<()> {
        self.vars
            .get_mut(name)
            .map(|v| *v = val)
            .ok_or(ExecError::NoSuchItem(name.to_string()))
    }

    fn list(&self) {
        for (name, val) in &self.vars {
            println!("{} = {:?}", name, val);
        }
    }

    fn as_scope_mut(&mut self) -> &mut dyn Scope {
        self
    }
}
