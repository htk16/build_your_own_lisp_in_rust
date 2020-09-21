use crate::environment::Environment;
use crate::error;
use crate::expression::ir::IR;
use crate::expression::{Evaluate, EvaluationResult, Expression, ExpressionType};
use crate::validation::{ArgumentsValidation, TypeValidation};
use ::phf::phf_map;
use anyhow::{anyhow, Result};
use std::rc::Rc;

// TODO move error module
macro_rules! panic_by_unexpected_arrival {
    () => {
        panic!("Unexpected arrival!")
    };
}

fn get_numbers(exprs: &[Expression]) -> Result<Vec<i64>> {
    exprs
        .iter()
        .map(|expr| {
            expr.number().ok_or(anyhow!(error::expression_type_error(
                ExpressionType::Integer,
                expr
            )))
        })
        .collect()
}

pub type BuiltinFunction = fn(&[Expression], &Environment) -> EvaluationResult;

pub fn plus(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    get_numbers(exprs)
        .map(|nums| nums.iter().fold(0, |acc, n| acc + n))
        .map(|v| (IR::Integer(v).into(), env.clone()))
}

fn minus(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    let numbers = get_numbers(exprs)?;
    let result = if numbers.len() == 1 {
        numbers[0] * -1
    } else {
        numbers[1..].iter().fold(numbers[0], |acc, n| acc - n)
    };
    Ok((IR::Integer(result).into(), env.clone()))
}

fn multiple(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    get_numbers(exprs)
        .map(|nums| nums.iter().fold(1, |acc, n| acc * n))
        .map(|v| (IR::Integer(v).into(), env.clone()))
}

fn divide(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    let numbers = get_numbers(exprs)?;
    if let Some(_) = numbers.iter().find(|v| **v == 0) {
        return Err(error::divide_by_zero());
    }

    let result = if numbers.len() == 1 {
        1 / numbers[0]
    } else {
        numbers[1..].iter().fold(numbers[0], |acc, n| acc / n)
    };
    Ok((IR::Integer(result).into(), env.clone()))
}

fn make_qexpr(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    let qlist_elems = exprs
        .iter()
        .map(|e| Rc::clone(e.as_ir_ref()))
        .collect::<Vec<_>>();
    Ok((IR::QExpr(qlist_elems).into(), env.clone()))
}

fn head_qexpr(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    ("head", exprs).required(1)?;
    (&exprs[0]).required_type(ExpressionType::QExpr)?;

    match exprs[0].as_ir() {
        IR::QExpr(xs) if xs.len() > 0 => {
            let head_elem = Rc::clone(&xs[0]);
            let head = IR::QExpr(vec![head_elem.into()]);
            Ok((head.into(), env.clone()))
        }
        IR::QExpr(xs) if xs.len() == 0 => Err(anyhow!("Function 'head' passed {}!")),
        _ => panic_by_unexpected_arrival!(),
    }
}

fn tail_qexpr(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    ("tail", exprs).required(1)?;
    (&exprs[0]).required_type(ExpressionType::QExpr)?;

    match exprs[0].as_ir() {
        IR::QExpr(xs) if xs.len() == 0 => Ok((exprs[0].as_ir_ref().into(), env.clone())),
        IR::QExpr(xs) if xs.len() > 0 => {
            let qexpr: Expression = IR::QExpr(xs[1..].iter().map(Rc::clone).collect()).into();
            Ok((qexpr, env.clone()))
        }
        _ => panic_by_unexpected_arrival!(),
    }
}

fn join_qexprs(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    let elems_list: Result<Vec<_>> = exprs
        .iter()
        .map(|expr| match expr.as_ir() {
            IR::QExpr(xs) => Ok(xs),
            _ => Err(error::expression_type_error(ExpressionType::QExpr, expr)),
        })
        .collect();
    let elems_list = elems_list?;
    let qexpr_elems: Vec<_> = elems_list
        .iter()
        .map(|xs| xs.iter())
        .flatten()
        .map(|x| Rc::clone(x))
        .collect();
    Ok((IR::QExpr(qexpr_elems.into()).into(), env.clone()))
}

fn eval_qexpr(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    ("eval", exprs).required(1)?;
    (&exprs[0]).required_type(ExpressionType::QExpr)?;

    match exprs[0].as_ir() {
        IR::QExpr(xs) => Expression::from(IR::SExpr(xs.to_vec())).evaluate(env),
        _ => panic_by_unexpected_arrival!(),
    }
}

fn define_symbol(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    ("define", exprs).required_more_than_or_equal(2)?;
    (&exprs[0]).required_type(ExpressionType::QExpr)?;

    if let IR::QExpr(es) = exprs[0].as_ir() {
        if exprs.len() - 1 != es.len() {
            return Err(error::argument_error("def", es.len() + 1, exprs.len()));
        };

        for e in es {
            // TODO rewrite using TypeValidation
            if !e.is_symbol() {
                return Err(error::expression_type_error(
                    ExpressionType::Symbol,
                    &Expression::from(e),
                ));
            }
        }

        let new_env = es
            .iter()
            .map(|symbol| symbol.symbol_name().unwrap())
            .zip(exprs[1..].iter())
            .fold(env.clone(), |e, (name, value)| {
                e.push(name.to_string(), Expression::from(value.as_ir_ref()))
            });
        Ok((Expression::from(IR::SExpr(vec![])), new_env))
    } else {
        panic_by_unexpected_arrival!()
    }
}

fn lambda(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    (r"\", exprs).required(2)?;
    (&exprs[0]).required_type(ExpressionType::QExpr)?;
    (&exprs[1]).required_type(ExpressionType::QExpr)?;

    match (exprs[0].as_ir(), exprs[1].as_ir()) {
        (IR::QExpr(_params), IR::QExpr(bs)) => {
            for param in _params {
                if !param.is_symbol() {
                    return Err(error::expression_type_error(
                        ExpressionType::Symbol,
                        &Expression::from(param),
                    ));
                }
            }

            let body = Expression::from(IR::SExpr(bs.iter().map(|e| e.clone()).collect()));
            let mut params: Vec<Expression> = _params
                .iter()
                .map(|ir_ref| Expression::from(ir_ref))
                .collect();
            let rest_symbol_pos = params
                .iter()
                .position(|sym| sym.symbol_name().unwrap_or("") == "&");
            match rest_symbol_pos {
                Some(i) => {
                    if (i != params.len() - 2)
                        || (params[params.len() - 1].symbol_name().unwrap_or("&") == "&")
                    {
                        Err(anyhow!("Illegal position of symbol &"))
                    } else {
                        // contains rest parameters
                        let rest = params[params.len() - 1].clone();
                        params.remove(i);
                        params.remove(i);
                        let lambda = IR::Lambda {
                            params,
                            body,
                            rests: Some(rest),
                            args: vec![],
                        };
                        Ok((lambda.into(), env.clone()))
                    }
                }
                None => {
                    let lambda = IR::Lambda {
                        params,
                        body,
                        rests: None,
                        args: vec![],
                    };
                    Ok((lambda.into(), env.clone()))
                }
            }
        }
        _ => panic_by_unexpected_arrival!(),
    }
}

pub static BUILTIN_FUNCTIONS: phf::Map<&'static str, BuiltinFunction> = {
    phf_map! {
        "+" => plus,
        "-" => minus,
        "*" => multiple,
        "/" => divide,
        "list" => make_qexpr,
        "head" => head_qexpr,
        "tail" => tail_qexpr,
        "join" => join_qexprs,
        "eval" => eval_qexpr,
        "def" => define_symbol,
        r"\" => lambda
    }
};
