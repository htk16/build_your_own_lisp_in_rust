use crate::expression::{Expression, IR, FunctionBody};
use crate::function;
use anyhow::Result;
use std::rc::Rc;
use std::borrow::Borrow;
use std::mem;

/// Environment
pub struct Environment {
    binding: Link
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        Environment::new(self.clone_binding())
    }
}

type Link = Option<Rc<Binding>>;

pub struct Binding {
    name: String,
    value: Expression,
    next: Link
}

// TODO replace IR::from
fn make_function(func: fn(&[Expression], &Environment) -> Result<(Expression, Environment)>) -> Expression {
    Expression::from(IR::Function(FunctionBody::new(func)))
}

impl Environment {
    pub fn new(binding: Link) -> Environment {
        Environment{ binding }
    }

    pub fn push(&self, name: String, value: Expression) -> Environment {
        let new_link = Some(Rc::new(Binding {name, value, next: self.clone_binding()}));
        Environment::new(new_link)
    }

    pub fn find<'a>(&'a self, name: &str) -> Option<&'a Expression> {
        self.into_iter()
            .find_map(|(n, v)| if name == n { Some(v) } else { None })
    }

    pub fn init() -> Environment {
        let init_env = Environment::new(None);
        function::BUILTIN_FUNCTIONS
            .entries()
            .fold(init_env, |env, (name, builtin)| env.push(name.to_string(), make_function(*builtin)))
    }

    fn clone_binding(&self) -> Link {
        self.binding.as_ref().map(|b| Rc::clone(&b))
    }
}

impl<'a> IntoIterator for &'a Environment {
    type Item = (&'a str, &'a Expression);
    type IntoIter = EnvironmentIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        EnvironmentIter{ next: self.binding.as_ref().map(|b| b.borrow()) }
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
