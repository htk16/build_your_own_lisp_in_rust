mod ir;
mod function;
use crate::environment::Environment;
use crate::parser::Ast;
use crate::expression::ir::{IR, IRRef, FunctionBody};
pub use crate::expression::function::{BuiltinFunction, BUILTIN_FUNCTIONS};
use anyhow::Result;
use std::rc::Rc;
use std::ops::Deref;

/// S-Expression
#[derive(Debug)]
pub struct Expression(IRRef);

impl Clone for Expression {
    fn clone(&self) -> Self {
        Expression(Rc::clone(self.as_ir_ref()))
    }
}

impl Deref for Expression {
    type Target = IRRef;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub type EvaluationResult = Result<(Expression, Environment)>;

impl Expression {
    pub fn new(ir_ref: IRRef) -> Self {
        Expression(ir_ref)
    }

    pub fn as_ir_ref(&self) -> &IRRef {
        &self.0
    }

    pub fn as_ir(&self) -> &IR {
        &*self.as_ir_ref()
    }

    pub fn make_function(func: BuiltinFunction) -> Expression {
        // TODO replace IR::from
        Expression::from(IR::Function(FunctionBody::new(func)))
    }
}

pub trait Evaluate {
    fn evaluate(&self, env: &Environment) -> EvaluationResult;
}

impl From<&Ast> for Expression {
    fn from(ast: &Ast) -> Self {
        IRRef::from(ast).into()
    }
}

#[cfg(test)]
mod tests {
    use super::{Expression, Evaluate, Environment};
    use crate::parser::parse;
    use anyhow::anyhow;

    fn create(i: &str) -> String {
        match parse(i).map(|(ast, _)| Expression::from(&ast)) {
            Ok(e) => e.to_string(),
            Err(e) => e.to_string(),
        }
    }

    #[test]
    fn create_expression() {
        assert_eq!("(+)", create("+"));
        assert_eq!("(+ 1 (* 2 3) (- 4 5))", create("+ 1 (* 2 3) (- 4 5)"));
        assert_eq!("((- 100))", create("(- 100)"));
        assert_eq!("()", create(""));
        assert_eq!("(/)", create("/"));
        assert_eq!("({1 2 (+ 5 6) 4})", create("{1 2 (+ 5 6) 4}"));
        assert_eq!("(list 1 2 3 4)", create("list 1 2 3 4"));
        assert_eq!("(eval {head (list 1 2 3 4)})", create("eval {head (list 1 2 3 4)}"));
        assert_eq!("(def {x} 100)", create("def {x} 100"));
        assert_eq!("(def {arglist} {a b x y})", create("def {arglist} {a b x y}"));
    }

    fn eval(i: &str) -> String {
        let env = Environment::init();
        parse(i)
            .map(|(ast, _)| Expression::from(&ast))
            .map_err(|err| anyhow!(err.to_string()))
            .and_then(|e| e.evaluate(&env))
            .map(|(e, _)| e.to_string())
            .unwrap_or_else(|err| err.to_string())
    }

    #[test]
    fn evaluate_expression() {
        assert_eq!("<function>", eval("+"));
        assert_eq!("6", eval("+ 1 (* 2 3) (- 4 5)"));
        assert_eq!("-100", eval("(- 100)"));
        assert_eq!("()", eval(""));
        assert_eq!("<function>", eval("/"));
        assert_eq!("{1 2 (+ 5 6) 4}", eval("{1 2 (+ 5 6) 4}"));
        assert_eq!("{1 2 3 4}", eval("list 1 2 3 4"));
        assert_eq!("1", eval("eval {head (list 1 2 3 4)}"));
        assert_eq!("()", eval("def {x} 100"));
        assert_eq!("()", eval("def {arglist} {a b x y}"));
    }
}
