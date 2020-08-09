use crate::parser::Ast;
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
    fn as_ir_ref(&self) -> &IRRef {
        &self.0
    }

    fn as_ir(&self) -> &IR {
        &*self.as_ir_ref()
    }

    fn symbol_name(&self) -> Option<&str> {
        self.as_ir().symbol_name()
    }

    fn type_name(&self) -> &'static str {
        self.as_ir().type_name()
    }
}

pub trait Evaluate {
    fn evaluate(&self) -> Result<Expression>;
}

impl Evaluate for Expression {
    fn evaluate(&self) -> Result<Expression> {
        self.as_ir_ref().evaluate()
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

/// Internal Representation of S-Expression
#[derive(Debug)]
enum IR {
    // Atoms
    Integer(i64),
    Symbol(String),

    // Lists
    SExpr(Vec<IRRef>),
    QExpr(Vec<IRRef>)
}

impl IR {
    fn symbol_name(&self) -> Option<&str> {
        match self {
            IR::Symbol(name) => Some(&name),
            _ => None
        }
    }

    fn type_name(&self) -> &'static str {
        match *self {
            IR::Integer(_) => "integer",
            IR::Symbol(_) => "symbol",
            IR::SExpr(_) => "s-expression",
            IR::QExpr(_) => "q-expression"
        }
    }
}

type IRRef = Rc<IR>;

impl ToString for IR {
    fn to_string(&self) -> String {
        match self {
            IR::Integer(i) => i.to_string(),
            IR::Symbol(s) => s.clone(),
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
    fn evaluate(&self) -> Result<Expression> {
        match &**self {
            IR::SExpr(exprs) => eval_slist(exprs.as_slice()),
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

fn eval_slist(exprs: &[IRRef]) -> Result<Expression> {
    let evaluated_exprs: Result<Vec<_>> = exprs.iter().map(|expr| expr.evaluate()).collect();
    let mut evaluated_exprs = evaluated_exprs?;
    match evaluated_exprs.len() {
        0 => Ok(IR::SExpr(vec![]).into()),
        1 => Ok(evaluated_exprs.pop().unwrap()),
        _ => apply_function(&evaluated_exprs)
    }
}

fn apply_function(exprs: &[Expression]) -> Result<Expression> {
    assert!(exprs.len() > 1);
    let head_symbol_name = exprs[0]
        .symbol_name()
        .ok_or(error::make_type_error("symbol", &exprs[0].to_string()))?;
    let function = functions::get_function(head_symbol_name)?;
    function(&exprs[1..])
}

mod functions {
    use std::rc::Rc;
    use ::phf::phf_map;
    use anyhow::{anyhow, Result};
    use super::{Expression, IR, Evaluate};
    use crate::error;

    fn get_numbers(exprs: &[Expression]) -> Result<Vec<i64>> {
        exprs.iter().map(Into::<Result<i64>>::into).collect()
    }

    type BuiltinFunction = fn(&[Expression]) -> Result<Expression>;

    fn plus(exprs: &[Expression]) -> Result<Expression> {
        get_numbers(exprs)
            .map(|nums| nums.iter().fold(0, |acc, n| acc + n))
            .map(|v| IR::Integer(v).into())
    }

    fn minus(exprs: &[Expression]) -> Result<Expression> {
        let numbers = get_numbers(exprs)?;
        let result = if numbers.len() == 1 {
            numbers[0] * -1
        } else {
            numbers[1..].iter().fold(numbers[0], |acc, n| acc - n)
        };
        Ok(IR::Integer(result).into())
    }

    fn multiple(exprs: &[Expression]) -> Result<Expression> {
        get_numbers(exprs)
            .map(|nums| nums.iter().fold(1, |acc, n| acc * n))
            .map(|v| IR::Integer(v).into())
    }

    fn divide(exprs: &[Expression]) -> Result<Expression> {
        let numbers = get_numbers(exprs)?;
        if let Some(_) = numbers.iter().find(|v| **v == 0) {
            return Err(anyhow!("Divide by zero"))
        }

        let result = if numbers.len() == 1 {
            1 / numbers[0]
        } else {
            numbers[1..].iter().fold(numbers[0], |acc, n| acc / n)
        };
        Ok(IR::Integer(result).into())
    }

    fn make_qexpr(exprs: &[Expression]) -> Result<Expression> {
        let qlist_elems = exprs
            .iter()
            .map(|e| Rc::clone(e.as_ir_ref()))
            .collect::<Vec<_>>();
        Ok(IR::QExpr(qlist_elems).into())
    }

    fn head_qexpr(exprs: &[Expression]) -> Result<Expression> {
        if exprs.len() != 1 { return Err(error::make_argument_error("head", 1, exprs.len())) }
        match exprs[0].as_ir() {
            IR::QExpr(xs) if xs.len() > 0 => Ok(Rc::clone(&xs[0]).into()),
            IR::QExpr(xs) if xs.len() == 0 => Err(anyhow!("Function 'head' passed {}!")),
            _ => Err(error::make_type_error("qexpr", &exprs[0].to_string()))
        }
    }

    fn tail_qexpr(exprs: &[Expression]) -> Result<Expression> {
        if exprs.len() != 1 { return Err(error::make_argument_error("tail", 1, exprs.len())) }
        match exprs[0].as_ir() {
            IR::QExpr(xs) if xs.len() == 0 => Ok(exprs[0].as_ir_ref().into()),
            IR::QExpr(xs) if xs.len() > 0 => {
                let qexpr: Expression = IR::QExpr(
                    xs[1..]
                        .iter()
                        .map(Rc::clone)
                        .collect()).into();
                Ok(qexpr)
            },
            _ => Err(error::make_type_error("qexpr", &exprs[0].to_string()))
        }
    }

    fn join_qexprs(exprs: &[Expression]) -> Result<Expression> {
        let elems_list: Result<Vec<_>> = exprs
            .iter()
            .map(|expr| match expr.as_ir() {
                IR::QExpr(xs) => Ok(xs),
                e => Err(error::make_type_error("qexpr", &(*e).to_string()))
            })
            .collect();
        let elems_list = elems_list?;
        let qexpr_elems: Vec<_> = elems_list
            .iter()
            .map(|xs| xs.iter())
            .flatten()
            .map(|x| Rc::clone(x))
            .collect();
        Ok(IR::QExpr(qexpr_elems.into()).into())
    }

    fn eval_qexpr(exprs: &[Expression]) -> Result<Expression> {
        if exprs.len() != 1 { return Err(error::make_argument_error("head", 1, exprs.len())) }
        match exprs[0].as_ir() {
            IR::QExpr(xs) => Expression::from(IR::SExpr(xs.to_vec())).evaluate(),
            _ => Err(error::make_type_error("qlist", &exprs[0].to_string()))
        }
    }

    static BUILTIN_FUNCTIONS: phf::Map<&'static str, BuiltinFunction> = {
        phf_map! {
            "+" => plus,
            "-" => minus,
            "*" => multiple,
            "/" => divide,
            "list" => make_qexpr,
            "head" => head_qexpr,
            "tail" => tail_qexpr,
            "join" => join_qexprs,
            "eval" => eval_qexpr
        }
    };

    pub fn get_function(name: &str) -> Result<BuiltinFunction> {
        BUILTIN_FUNCTIONS
            .get(name)
            .map(|ref_fn| *ref_fn)
            .ok_or(anyhow!("Not found function '{}'", name))
    }
}

#[cfg(test)]
mod tests {
    use super::{Expression, Evaluate};
    use crate::parser::parse;
    use anyhow::anyhow;
    use nom::error::ErrorKind;

    fn create(i: &str) -> String {
        match parse::<(&str, ErrorKind)>(i).map(|(_, ast)| Expression::from(&ast)) {
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
    }

    fn eval(i: &str) -> String {
        parse::<(&str, ErrorKind)>(i)
            .map(|(_, ast)| Expression::from(&ast))
            .map_err(|err| anyhow!(err.to_string()))
            .and_then(|e| e.evaluate())
            .map(|e| e.to_string())
            .unwrap_or_else(|err| err.to_string())
    }

    #[test]
    fn evaluate_expression() {
        assert_eq!("+", eval("+"));
        assert_eq!("6", eval("+ 1 (* 2 3) (- 4 5)"));
        assert_eq!("-100", eval("(- 100)"));
        assert_eq!("()", eval(""));
        assert_eq!("/", eval("/"));
        assert_eq!("{1 2 (+ 5 6) 4}", eval("{1 2 (+ 5 6) 4}"));
        assert_eq!("{1 2 3 4}", eval("list 1 2 3 4"));
        assert_eq!("1", eval("eval {head (list 1 2 3 4)}"));
    }
}
