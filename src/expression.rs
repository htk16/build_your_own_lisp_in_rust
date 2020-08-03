use crate::parser::Ast;
use anyhow::{anyhow, Result};
use std::rc::Rc;

/// S-Expression
#[derive(Debug)]
pub struct Expression(IRRef);

/// Internal Representation of S-Expression
#[derive(Debug)]
enum IR {
    // Atoms
    Nil,
    Integer(i64),
    Symbol(String),

    // Lists
    Cons(IRRef, IRRef),
}

type IRRef = Rc<IR>;

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
            Ast::List(xs) => {
                let expr = xs
                    .iter()
                    .rev()
                    .fold(IR::Nil, |acc, x| IR::Cons(IRRef::from(x), Rc::new(acc)));
                Rc::new(expr)
            }
        }
    }
}

struct ConsIterator {
    cons: IRRef,
}

impl Iterator for ConsIterator {
    type Item = IRRef;
    fn next(&mut self) -> Option<Self::Item> {
        let cons = Rc::clone(&self.cons);
        match &*cons {
            IR::Cons(car, cdr) => {
                self.cons = Rc::clone(&cdr);
                Some(Rc::clone(&car))
            }
            _ => None,
        }
    }
}

impl Clone for Expression {
    fn clone(&self) -> Self {
        Expression(Rc::clone(self.as_ir_ref()))
    }
}

impl Into<Expression> for IRRef {
    fn into(self) -> Expression {
        Expression(Rc::clone(&self))
    }
}

impl Into<Expression> for &IRRef {
    fn into(self) -> Expression {
        Expression(Rc::clone(self))
    }
}

impl Into<Expression> for IR {
    fn into(self) -> Expression {
        Rc::new(self).into()
    }
}

impl Expression {
    fn as_ir_ref(&self) -> &IRRef {
        &self.0
    }

    fn as_ir(&self) -> &IR {
        &*self.as_ir_ref()
    }

    fn iter(&self) -> ConsIterator {
        ConsIterator {
            cons: Rc::clone(self.as_ir_ref()),
        }
    }

    pub fn evaluate(&self) -> Result<Expression> {
        match self.as_ir() {
            IR::Nil => Ok(self.clone()),
            IR::Integer(_) => Ok(self.clone()),
            IR::Symbol(name) => Err(anyhow!("Evaluating symbol(name: {}) isn't supported", name)),
            IR::Cons(car, cdr) => apply_operator(&car, &cdr),
        }
    }
}

fn apply_operator(car: &IRRef, cdr: &IRRef) -> Result<Expression> {
    match &**car {
        IR::Symbol(op) => {
            let operation: Result<(i64, Box<dyn Fn(i64, i64) -> i64>)> = match op.as_str() {
                "+" => Ok((0, Box::new(|lhs, rhs| lhs + rhs))),
                "-" => Ok((0, Box::new(|lhs, rhs| lhs - rhs))),
                "*" => Ok((1, Box::new(|lhs, rhs| lhs * rhs))),
                "/" => Ok((1, Box::new(|lhs, rhs| lhs / rhs))),
                _ => Err(anyhow!("Unsupported operator: {}", op)),
            };
            let (init_value, function) = operation?;
            let cdr_expr: Expression = cdr.into();
            let evaluated_values: Result<Vec<_>> = cdr_expr
                .iter()
                .map(|ir_ref| Into::<Expression>::into(&ir_ref))
                .map(|expr| expr.evaluate())
                .collect();
            let evaluated_values = evaluated_values?;
            let values: Result<Vec<_>> = evaluated_values
                .iter()
                .map(Into::<Result<i64>>::into)
                .collect();
            let values = values?;
            let sum = if values.len() < 2 {
                values.iter().fold(init_value, |acc, v| function(acc, *v))
            } else {
                let first = values[0];
                (&values[1..])
                    .iter()
                    .fold(first, |acc, v| function(acc, *v))
            };
            Ok(IR::Integer(sum).into())
        }
        e => Err(anyhow!("Invalid symbol: {:?}", e)),
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
