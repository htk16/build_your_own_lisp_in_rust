mod function;
mod ir;
use crate::environment::Environment;
pub use crate::expression::function::{BuiltinFunction, BUILTIN_FUNCTIONS};
use crate::expression::ir::{FunctionBody, IRRef, IR};
use crate::parser::Ast;
use anyhow::Result;
use std::ops::Deref;
use std::rc::Rc;

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

    pub fn make_nil() -> Expression {
        Expression::from(IR::SExpr(vec![]))
    }

    pub fn make_function(func: BuiltinFunction) -> Expression {
        // TODO replace IR::from
        Expression::from(IR::Function(FunctionBody::new(func)))
    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum ExpressionType {
    Integer,
    Symbol,
    Function,
    SExpr,
    QExpr,
}

impl ExpressionType {
    pub fn name(&self) -> &'static str {
        match *self {
            ExpressionType::Integer => "integer",
            ExpressionType::Symbol => "symbol",
            ExpressionType::Function => "function",
            ExpressionType::SExpr => "s-expression",
            ExpressionType::QExpr => "q-expression",
        }
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
    use super::{Environment, Evaluate, Expression};
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
        assert_eq!(
            "(eval {head (list 1 2 3 4)})",
            create("eval {head (list 1 2 3 4)}")
        );
        assert_eq!("(def {x} 100)", create("def {x} 100"));
        assert_eq!(
            "(def {arglist} {a b x y})",
            create("def {arglist} {a b x y}")
        );
        assert_eq!(
            "(def {add-mul} (\\ {x y} {+ x (* x y)}))",
            create("def {add-mul} (\\ {x y} {+ x (* x y)})")
        );
        assert_eq!("(> 10 5)", create("> 10 5"));
        assert_eq!("(== 5 {})", create("== 5 {}"));
        assert_eq!("(== {1 2 3 {5 6}} {1 2 3 {5 6}})", create("== {1 2 3 {5 6}} {1   2  3   {5 6}}"));
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

    fn eval_codes(is: &Vec<&str>) -> String {
        let parse_result: Option<Vec<Expression>> = is
            .iter()
            .map(|i| parse(*i).ok().map(|(ast, _)| Expression::from(&ast)))
            .collect::<Option<Vec<Expression>>>();
        match parse_result {
            Some(es) => {
                let nil = Expression::make_nil();
                let init_env = Environment::init();
                es.iter()
                    .fold(Ok((nil, init_env)), |acc, expr| {
                        acc.and_then(|(_, env)| expr.evaluate(&env))
                    })
                    .map(|(expr, _)| expr.to_string())
                    .unwrap_or("evaluation error".to_string())
            }
            None => "parse error".to_string(),
        }
    }

    const FUN: &str = r"def {fun} (\ {args body} {def (head args) (\ (tail args) body)})";
    const PACK: &str = "fun {pack f & xs} {f xs}";
    const UNPACK: &str = "fun {unpack f xs} {eval (join (list f) xs)}";

    #[test]
    fn evaluate_expression() {
        assert_eq!("<function>", eval("+"));
        assert_eq!("6", eval("+ 1 (* 2 3) (- 4 5)"));
        assert_eq!("-100", eval("(- 100)"));
        assert_eq!("()", eval(""));
        assert_eq!("<function>", eval("/"));
        assert_eq!("{1 2 (+ 5 6) 4}", eval("{1 2 (+ 5 6) 4}"));
        assert_eq!("{1 2 3 4}", eval("list 1 2 3 4"));
        assert_eq!("{1}", eval("eval {head (list 1 2 3 4)}"));
        assert_eq!("()", eval("def {x} 100"));
        assert_eq!("()", eval("def {arglist} {a b x y}"));
        assert_eq!("()", eval("def {add-mul} (\\ {x y} {+ x (* x y)})"));
        assert_eq!(
            "210",
            eval_codes(&vec![
                "def {add-mul} (\\ {x y} {+ x (* x y)})",
                "add-mul 10 20"
            ])
        );
        assert_eq!(
            "3",
            eval_codes(&vec![
                FUN,
                "fun {add-together x y} {+ x y}",
                "(add-together 1) 2"
            ])
        );
        assert_eq!("18", eval_codes(&vec![FUN, UNPACK, "unpack + {5 6 7}"]));
        assert_eq!("{5}", eval_codes(&vec![FUN, PACK, "pack head 5 6 7"]));
        assert_eq!("1", eval("> 10 5"));
        assert_eq!("0", eval("== 5 {}"));
        assert_eq!("1", eval("== {1 2 3 {5 6}} {1   2  3   {5 6}}"));
    }
}
