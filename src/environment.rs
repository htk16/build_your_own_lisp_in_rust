use crate::expression::{Expression, IR, FunctionBody};
use crate::function;
use anyhow::Result;
use std::rc::Rc;
use std::borrow::Borrow;
use std::mem;

/// Environment
pub struct Environment {
    bindings: Link
}

type Link = Option<Rc<Binding>>;

struct Binding {
    name: String,
    value: Expression,
    next: Link
}

// TODO replace IR::from
fn make_function(func: fn(&[Expression], &mut Environment) -> Result<Expression>) -> Expression {
    Expression::from(IR::Function(FunctionBody::new(func)))
}

impl Environment {
    pub fn new() -> Environment {
        Environment{ bindings: None }
    }

    pub fn push(&mut self, name: String, value: Expression) {
        self.bindings = Some(Rc::new(Binding { name, value, next: mem::replace(&mut self.bindings, None) }));
    }

    pub fn find<'a>(&'a self, name: &str) -> Option<&'a Expression> {
        self.into_iter()
            .find_map(|(n, v)| if name == n { Some(v) } else { None })
    }

    pub fn init() -> Environment {
        let mut env = Environment::new();
        for (name, builtin) in function::BUILTIN_FUNCTIONS.entries() {
            env.push(name.to_string(), make_function(*builtin))
        }
        env
    }
}

impl<'a> IntoIterator for &'a Environment {
    type Item = (&'a str, &'a Expression);
    type IntoIter = EnvironmentIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        EnvironmentIter{ next: self.bindings.as_ref().map(|b| b.borrow()) }
    }
}

pub struct EnvironmentIter<'a> {
    next: Option<&'a Binding>
}

impl<'a> Iterator for EnvironmentIter<'a> {
    type Item = (&'a str, &'a Expression);

    fn next(&mut self) -> Option<Self::Item> {
        let next_binding = self.next
            .as_ref()
            .and_then(|binding| binding.next.as_ref().map(|b| b.borrow()));
        let item = self.next.as_ref().map(|binding| (binding.name.as_ref(), &binding.value));
        self.next = next_binding;
        item
    }
}
