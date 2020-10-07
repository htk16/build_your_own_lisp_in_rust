use crate::environment::Environment;
use crate::error;
use crate::parser;
use crate::expression::ir::{IRRef, IR};
use crate::expression::{Evaluate, EvaluationResult, Expression, ExpressionType};
use crate::validation::{ArgumentsValidation, TypeValidation};
use ::phf::phf_map;
use anyhow::{anyhow, Result};
use combine::EasyParser;
use combine::stream::position::Stream;
use std::rc::Rc;
use std::fs;

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

fn plus(exprs: &[Expression], env: &Environment) -> EvaluationResult {
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

fn equal_expression(lhs: &Expression, rhs: &Expression) -> bool {
    match (lhs.as_ir(), rhs.as_ir()) {
        (IR::Integer(l), IR::Integer(r)) => l == r,
        (IR::Symbol(l), IR::Symbol(r)) => l == r,
        (IR::Function(l), IR::Function(r)) => *l == *r,
        (
            IR::Lambda {
                params: lparams,
                rests: lrests,
                body: lbody,
                args: largs,
            },
            IR::Lambda {
                params: rparams,
                rests: rrests,
                body: rbody,
                args: rargs,
            },
        ) => {
            equal_expressions(lparams, rparams)
                && equal_op_expression(lrests, rrests)
                && equal_expression(lbody, rbody)
                && equal_expressions(largs, rargs)
        }
        (IR::SExpr(l), IR::SExpr(r)) => equal_ir_refs(l, r),
        (IR::QExpr(l), IR::QExpr(r)) => equal_ir_refs(l, r),
        _ => false,
    }
}

fn equal_op_expression(lhs: &Option<Expression>, rhs: &Option<Expression>) -> bool {
    match (lhs, rhs) {
        (Some(l), Some(r)) => equal_expression(l, r),
        (None, None) => true,
        _ => false,
    }
}

fn equal_expressions(lhs: &[Expression], rhs: &[Expression]) -> bool {
    if lhs.len() != rhs.len() {
        return false;
    };
    lhs.iter()
        .zip(rhs.iter())
        .all(|(l, r)| equal_expression(l, r))
}

fn equal_ir_refs(lhs: &[IRRef], rhs: &[IRRef]) -> bool {
    if lhs.len() != rhs.len() {
        return false;
    };
    lhs.iter()
        .zip(rhs.iter())
        .all(|(l, r)| equal_expression(&Expression::from(l), &Expression::from(r)))
}

fn equal(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    (r"\", exprs).required(2)?;
    Ok((
        IR::Integer(if equal_expression(&exprs[0], &exprs[1]) {
            1
        } else {
            0
        })
        .into(),
        env.clone(),
    ))
}

fn not_equal(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    (r"\", exprs).required(2)?;
    Ok((
        IR::Integer(if !equal_expression(&exprs[0], &exprs[1]) {
            1
        } else {
            0
        })
        .into(),
        env.clone(),
    ))
}

macro_rules! integer_binary_predicate {
    ($name: ident, $op: tt) => {
        fn $name(exprs: &[Expression], env: &Environment) -> EvaluationResult {
            (r"\", exprs).required(2)?;
            (&exprs[0]).required_type(ExpressionType::Integer)?;
            (&exprs[1]).required_type(ExpressionType::Integer)?;

            match (exprs[0].number(), exprs[1].number()) {
                (Some(lhs), Some(rhs)) => Ok((IR::Integer(if lhs $op rhs {1} else {0}).into(), env.clone())),
                _ => panic_by_unexpected_arrival!()
            }
        }
    };
}
integer_binary_predicate!(less_than, <);
integer_binary_predicate!(less_than_equal, <=);
integer_binary_predicate!(more_than, >);
integer_binary_predicate!(more_than_equal, >=);

fn if_(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    (r"\", exprs).required(3)?;
    (&exprs[0]).required_type(ExpressionType::Integer)?;
    (&exprs[1]).required_type(ExpressionType::QExpr)?;
    (&exprs[2]).required_type(ExpressionType::QExpr)?;

    exprs[0]
        .number()
        .ok_or_else(|| error::expression_type_error(ExpressionType::Integer, &exprs[0]))
        .and_then(|v| {
            if v != 0 {
                eval_qexpr(&exprs[1..2], env)
            } else {
                eval_qexpr(&exprs[2..3], env)
            }
        })
}

fn load(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    ("error", exprs).required(1)?;
    (&exprs[0]).required_type(ExpressionType::String_)?;

    if let IR::String_(filepath) = exprs[0].as_ir() {
        let contents = fs::read_to_string(filepath)?;
        let stream = Stream::new(contents.as_str());
        let mut ast_parser = parser::root_for_load();
        let parse_result = ast_parser.easy_parse(stream);
        match parse_result {
            Ok((asts, _rest)) => {
                println!("AST: {:?}", asts);
                let mut current_env = env.clone();
                for ast in asts {
                    let ir_ref = IRRef::from(&ast);
                    let (_, new_env) = ir_ref.evaluate(&current_env)?;
                    current_env = new_env;
                }
                Ok((Expression::make_nil(), current_env))
            },
            Err(parse_error) => {
                Err(anyhow!("Parse error: {}", parse_error.to_string()))
            }
        }
    } else {
        panic_by_unexpected_arrival!()
    }
}

fn print(exprs: &[Expression], env: &Environment) -> EvaluationResult {
    let strings = exprs
        .iter()
        .map(|e| if let IR::String_(s) = e.as_ir() { Ok(s)} else { Err(error::expression_type_error(ExpressionType::String_, e)) })
        .collect::<Result<Vec<_>>>()?;
    let contents = itertools::join(strings, " ");
    println!("{}", contents);
    Ok((Expression::make_nil(), env.clone()))
}

fn error_(exprs: &[Expression], _env: &Environment) -> EvaluationResult {
    ("error", exprs).required(1)?;
    (&exprs[0]).required_type(ExpressionType::String_)?;

    if let IR::String_(msg) = exprs[0].as_ir() {
        Err(anyhow!(msg.to_string()))
    } else {
        panic_by_unexpected_arrival!()
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
        r"\" => lambda,
        r"<" => less_than,
        r"<=" => less_than_equal,
        r">" => more_than,
        r">=" => more_than_equal,
        r"==" => equal,
        r"!=" => not_equal,
        "if" => if_,
        "load" => load,
        "print" => print,
        "error" => error_
    }
};
