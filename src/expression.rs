use crate::parser::Ast;
use crate::environment::Environment;
use crate::error;
use anyhow::{anyhow, Result};
use std::rc::Rc;

/// S-Expression
#[derive(Debug)]
pub struct Expression(IRRef);

impl Clone for Expression {
    fn clone(&self) -> Self {
        Expression(Rc::clone(self.as_ir_ref()))
    }
}

impl Expression {
    pub fn as_ir_ref(&self) -> &IRRef {
        &self.0
    }

    pub fn as_ir(&self) -> &IR {
        &*self.as_ir_ref()
    }

    pub fn symbol_name(&self) -> Option<&str> {
        self.as_ir().symbol_name()
    }

    pub fn type_name(&self) -> &'static str {
        self.as_ir().type_name()
    }
}

pub trait Evaluate {
    fn evaluate(&self, env: &mut Environment) -> Result<Expression>;
}

impl Evaluate for Expression {
    fn evaluate(&self, env: &mut Environment) -> Result<Expression> {
        self.as_ir_ref().evaluate(env)
    }
}

impl ToString for Expression {
    fn to_string(&self) -> String {
        self.as_ir().to_string()
    }
}

impl From<&Expression> for Result<i64> {
    fn from(expr: &Expression) -> Result<i64> {
        match expr.as_ir() {
            IR::Integer(v) => Ok(*v),
            _ => Err(anyhow!("{:?} isn't integer", expr.as_ir())),
        }
    }
}

pub struct FunctionBody(Box<dyn Fn(&[Expression], &mut Environment) -> Result<Expression>>);

impl FunctionBody {
    pub fn new(func: impl Fn(&[Expression], &mut Environment) -> Result<Expression> + 'static) -> FunctionBody {
        FunctionBody(Box::new(func))
    }
}

impl std::fmt::Debug for FunctionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("FunctionBody").finish()
    }
}

/// Internal Representation of S-Expression
#[derive(Debug)]
pub enum IR {
    // Atoms
    Integer(i64),
    Symbol(String),
    Function(FunctionBody),

    // Lists
    SExpr(Vec<IRRef>),
    QExpr(Vec<IRRef>)
}

impl IR {
    pub fn symbol_name(&self) -> Option<&str> {
        match self {
            IR::Symbol(name) => Some(&name),
            _ => None
        }
    }

    pub fn type_name(&self) -> &'static str {
        match *self {
            IR::Integer(_) => "integer",
            IR::Symbol(_) => "symbol",
            IR::Function(_) => "function",
            IR::SExpr(_) => "s-expression",
            IR::QExpr(_) => "q-expression"
        }
    }

    pub fn is_symbol(&self) -> bool {
        if let IR::Symbol(_) = *self {true} else {false}
    }

    pub fn is_sexpr(&self) -> bool {
        if let IR::SExpr(_) = *self {true} else {false}
    }

    pub fn is_qexpr(&self) -> bool {
        if let IR::QExpr(_) = *self {true} else {false}
    }

    pub fn is_function(&self) -> bool {
        if let IR::Function(_) = *self {true} else {false}
    }
}

type IRRef = Rc<IR>;

impl ToString for IR {
    fn to_string(&self) -> String {
        match self {
            IR::Integer(i) => i.to_string(),
            IR::Symbol(s) => s.clone(),
            IR::Function(_) => "<function>".to_string(),
            IR::SExpr(exprs) => format!(
                "({})",
                itertools::join(
                    exprs.iter().map(|ir_ref| Expression::from(ir_ref).to_string()),
                    " ")
            ),
            IR::QExpr(exprs) => format!(
                "{{{}}}",
                itertools::join(
                    exprs.iter().map(|ir_ref| Expression::from(ir_ref).to_string()),
                    " ")
            )
        }
    }
}

impl Evaluate for IRRef {
    fn evaluate(&self, env: &mut Environment) -> Result<Expression> {
        match &**self {
            IR::SExpr(exprs) => eval_slist(exprs.as_slice(), env),
            IR::Symbol(name) => env
                .find(name.as_ref())
                .map(|v| Expression(Rc::clone(v.as_ir_ref())))
                .ok_or(anyhow!(format!("Unable to resolve symbol: {} in this context", name))),
            // self evaluation forms
            _ => Ok(Rc::clone(self).into())
        }
    }
}

impl From<&Ast> for Expression {
    fn from(ast: &Ast) -> Self {
        IRRef::from(ast).into()
    }
}

impl From<&Ast> for IRRef {
    fn from(ast: &Ast) -> Self {
        match ast {
            Ast::Integer(v) => Rc::new(IR::Integer(*v)),
            Ast::Symbol(n) => Rc::new(IR::Symbol(String::from(n))),
            Ast::SExpr(xs) => {
                let expr = xs
                    .iter()
                    .map(|x| IRRef::from(x))
                    .collect::<Vec<_>>();
                Rc::new(IR::SExpr(expr))
            }
            Ast::QExpr(xs) => {
                let expr = xs
                    .iter()
                    .map(|x| IRRef::from(x))
                    .collect::<Vec<_>>();
                Rc::new(IR::QExpr(expr))
            }
        }
    }
}

impl From<IRRef> for Expression {
    fn from(from: IRRef) -> Expression {
        Expression(from)
    }
}

impl From<&IRRef> for Expression {
    fn from(from: &IRRef) -> Expression {
        Expression(Rc::clone(from))
    }
}

impl From<IR> for Expression {
    fn from(from: IR) -> Expression {
        let ir_ref = Rc::new(from);
        Expression::from(ir_ref)
    }
}

fn eval_slist(exprs: &[IRRef], env: &mut Environment) -> Result<Expression> {
    match exprs.len() {
        0 => Ok(IR::SExpr(vec![]).into()),
        1 => exprs[0].evaluate(env),
        _ => exprs.iter()
            .map(|expr| expr.evaluate(env))
            .collect::<Result<Vec<_>>>()
            .and_then(|es| apply_function(&es, env))
    }
}

fn apply_function(exprs: &[Expression], env: &mut Environment) -> Result<Expression> {
    assert!(exprs.len() > 0);
    if let IR::Function(func) = exprs[0].as_ir() {
        func.0(&exprs[1..], env)
    } else {
        Err(error::make_type_error("function", exprs[0].as_ir().type_name()))
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
        let mut env = Environment::init();
        parse(i)
            .map(|(ast, _)| Expression::from(&ast))
            .map_err(|err| anyhow!(err.to_string()))
            .and_then(|e| e.evaluate(&mut env))
            .map(|e| e.to_string())
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
