use std::rc::Rc;
use ::phf::phf_map;
use anyhow::{anyhow, Result};
use crate::expression::{Expression, IR, Evaluate};
use crate::environment::Environment;
use crate::error;

fn get_numbers(exprs: &[Expression]) -> Result<Vec<i64>> {
    exprs.iter().map(Into::<Result<i64>>::into).collect()
}

type BuiltinFunction = fn(&[Expression], &mut Environment) -> Result<Expression>;

pub fn plus(exprs: &[Expression], _env: &mut Environment) -> Result<Expression> {
    get_numbers(exprs)
        .map(|nums| nums.iter().fold(0, |acc, n| acc + n))
        .map(|v| IR::Integer(v).into())
}

fn minus(exprs: &[Expression], _env: &mut Environment) -> Result<Expression> {
    let numbers = get_numbers(exprs)?;
    let result = if numbers.len() == 1 {
        numbers[0] * -1
    } else {
        numbers[1..].iter().fold(numbers[0], |acc, n| acc - n)
    };
    Ok(IR::Integer(result).into())
}

fn multiple(exprs: &[Expression], _env: &mut Environment) -> Result<Expression> {
    get_numbers(exprs)
        .map(|nums| nums.iter().fold(1, |acc, n| acc * n))
        .map(|v| IR::Integer(v).into())
}

fn divide(exprs: &[Expression], _env: &mut Environment) -> Result<Expression> {
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

fn make_qexpr(exprs: &[Expression], _env: &mut Environment) -> Result<Expression> {
    let qlist_elems = exprs
        .iter()
        .map(|e| Rc::clone(e.as_ir_ref()))
        .collect::<Vec<_>>();
    Ok(IR::QExpr(qlist_elems).into())
}

fn head_qexpr(exprs: &[Expression], _env: &mut Environment) -> Result<Expression> {
    if exprs.len() != 1 { return Err(error::make_argument_error("head", 1, exprs.len())) }
    match exprs[0].as_ir() {
        IR::QExpr(xs) if xs.len() > 0 => Ok(Rc::clone(&xs[0]).into()),
        IR::QExpr(xs) if xs.len() == 0 => Err(anyhow!("Function 'head' passed {}!")),
        _ => Err(error::make_type_error("qexpr", &exprs[0].to_string()))
    }
}

fn tail_qexpr(exprs: &[Expression], _env: &mut Environment) -> Result<Expression> {
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

fn join_qexprs(exprs: &[Expression], _env: &mut Environment) -> Result<Expression> {
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

fn eval_qexpr(exprs: &[Expression], env: &mut Environment) -> Result<Expression> {
    if exprs.len() != 1 { return Err(error::make_argument_error("head", 1, exprs.len())) }
    match exprs[0].as_ir() {
        IR::QExpr(xs) => Expression::from(IR::SExpr(xs.to_vec())).evaluate(env),
        _ => Err(error::make_type_error("qlist", &exprs[0].type_name()))
    }
}

fn define_symbol(exprs: &[Expression], env: &mut Environment) -> Result<Expression> {
    if exprs.len() < 2 {
        return Err(anyhow!("Function 'def' required 2 or more arguments, but passed {}", exprs.len()))
    }

    if let IR::QExpr(es) = exprs[0].as_ir() {
        if exprs.len() - 1 != es.len() {
            return Err(error::make_argument_error("def", es.len() + 1, exprs.len()))
        };

        for e in es {
            if !e.is_symbol() {
                return Err(error::make_type_error("symbol", e.type_name()))
            }
        };

        es.iter()
            .map(|symbol| symbol.symbol_name().unwrap())
            .zip(exprs[1..].iter())
            .for_each(|(name, value)| env.push(name.to_string(), Expression::from(value.as_ir_ref())));
        Ok(Expression::from(IR::SExpr(vec![])))
    } else {
        Err(error::make_type_error("qlist", &exprs[0].type_name()))
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
    }
};

pub fn get_function(name: &str) -> Result<BuiltinFunction> {
    BUILTIN_FUNCTIONS
        .get(name)
        .map(|ref_fn| *ref_fn)
        .ok_or(anyhow!("Not found function '{}'", name))
}
