use crate::environment::Environment;
use crate::error;
use crate::expression::{Evaluate, EvaluationResult, Expression, ExpressionType, BuiltinFunction};
use crate::parser::Ast;
use anyhow::{anyhow, Result};
use std::rc::Rc;

pub struct FunctionBody(BuiltinFunction);

impl FunctionBody {
    pub fn new(func: BuiltinFunction) -> Self {
        FunctionBody(func)
    }
}

impl From<BuiltinFunction> for FunctionBody {
    fn from(func: BuiltinFunction) -> Self {
        FunctionBody(func)
    }
}

impl PartialEq<FunctionBody> for FunctionBody {
    fn eq(&self, other: &FunctionBody) -> bool {
        let lhs = self.0 as *const ();
        let rhs = other.0 as *const ();
        lhs == rhs
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
    Lambda {
        params: Vec<Expression>,
        rests: Option<Expression>,
        body: Expression,
        args: Vec<Expression>,
    },

    // Lists
    SExpr(Vec<IRRef>),
    QExpr(Vec<IRRef>),
}

impl IR {
    pub fn type_(&self) -> ExpressionType {
        match self {
            IR::Integer(_) => ExpressionType::Integer,
            IR::Symbol(_) => ExpressionType::Symbol,
            IR::Function(_) => ExpressionType::Function,
            IR::Lambda {
                params: _,
                rests: _,
                body: _,
                args: _,
            } => ExpressionType::Function,
            IR::SExpr(_) => ExpressionType::SExpr,
            IR::QExpr(_) => ExpressionType::QExpr,
        }
    }

    pub fn type_name(&self) -> &'static str {
        self.type_().name()
    }

    pub fn is_number(&self) -> bool {
        self.type_() == ExpressionType::Integer
    }

    pub fn is_symbol(&self) -> bool {
        self.type_() == ExpressionType::Symbol
    }

    pub fn is_sexpr(&self) -> bool {
        self.type_() == ExpressionType::SExpr
    }

    pub fn is_qexpr(&self) -> bool {
        self.type_() == ExpressionType::QExpr
    }

    pub fn is_function(&self) -> bool {
        self.type_() == ExpressionType::Function
    }

    pub fn is_atom(&self) -> bool {
        match self {
            IR::Integer(_) | IR::Symbol(_) | IR::Function(_) => true,
            IR::SExpr(es) => es.len() == 0,
            _ => false,
        }
    }

    pub fn symbol_name(&self) -> Option<&str> {
        if let IR::Symbol(name) = &*self {
            Some(&name)
        } else {
            None
        }
    }

    pub fn number(&self) -> Option<i64> {
        if let IR::Integer(i) = *self {
            Some(i)
        } else {
            None
        }
    }

    fn eval_slist(exprs: &[IRRef], env: &Environment) -> EvaluationResult {
        match exprs.len() {
            0 => Ok((Expression::make_nil(), env.clone())),
            1 => exprs[0].evaluate(env),
            _ => exprs
                .iter()
                .map(|expr| expr.evaluate(env))
                .collect::<Result<Vec<_>>>()
                .map(|rs| {
                    rs.iter()
                        .map(|(expr, _)| Expression::clone(expr))
                        .collect::<Vec<_>>()
                })
                .and_then(|es| IR::apply_function(es.as_slice(), env)),
        }
    }

    fn apply_function(exprs: &[Expression], env: &Environment) -> EvaluationResult {
        assert!(exprs.len() > 0);
        match exprs[0].as_ir() {
            IR::Function(func) => func.0(&exprs[1..], env),
            IR::Lambda {
                params,
                rests,
                body,
                args: applied_args,
            } => IR::apply_lambda(params, rests, body, applied_args, exprs[1..].into(), env),
            _ => Err(error::expression_type_error(
                ExpressionType::Function,
                &exprs[0],
            )),
        }
    }

    fn apply_lambda(
        params: &Vec<Expression>,
        rests: &Option<Expression>,
        body: &Expression,
        applied_args: &Vec<Expression>,
        args: Vec<Expression>,
        env: &Environment,
    ) -> EvaluationResult {
        let mut _exprs = args.clone();
        let mut new_args = applied_args.clone();
        new_args.append(&mut _exprs);

        if params.len() > new_args.len() {
            // partial evaluation
            let lambda: Expression = (IR::Lambda {
                params: params.clone(),
                rests: rests.clone(),
                body: body.clone(),
                args: new_args,
            })
            .into();
            Ok((lambda, env.clone()))
        } else if let Some(rest_sym) = &*rests {
            // full evaluation with rest parameters
            let local_env = params
                .iter()
                .zip(new_args.iter())
                .fold(env.clone(), |e, (param, arg)| {
                    e.push(param.symbol_name().unwrap().to_string(), arg.clone())
                });
            let rest_params: Vec<IRRef> = new_args[params.len()..]
                .iter()
                .map(|e| e.as_ir_ref().clone())
                .collect();
            let local_env = local_env.push(
                rest_sym.symbol_name().unwrap().to_string(),
                IR::QExpr(rest_params).into(),
            );
            body.evaluate(&local_env)
        } else if params.len() == new_args.len() {
            // full evaluation without rest parameters
            let local_env = params
                .iter()
                .zip(new_args.iter())
                .fold(env.clone(), |e, (param, arg)| {
                    e.push(param.symbol_name().unwrap().to_string(), arg.clone())
                });
            body.evaluate(&local_env)
        } else {
            Err(error::argument_error(
                "lambda",
                params.len(),
                new_args.len(),
            ))
        }
    }
}

pub type IRRef = Rc<IR>;

impl ToString for IR {
    fn to_string(&self) -> String {
        match self {
            IR::Integer(i) => i.to_string(),
            IR::Symbol(s) => s.clone(),
            IR::Function(_) => "<function>".to_string(),
            IR::Lambda {
                params: _,
                rests: _,
                body: _,
                args: _,
            } => "<lambda>".to_string(),
            IR::SExpr(exprs) => format!(
                "({})",
                itertools::join(
                    exprs
                        .iter()
                        .map(|ir_ref| Expression::from(ir_ref).to_string()),
                    " "
                )
            ),
            IR::QExpr(exprs) => format!(
                "{{{}}}",
                itertools::join(
                    exprs
                        .iter()
                        .map(|ir_ref| Expression::from(ir_ref).to_string()),
                    " "
                )
            ),
        }
    }
}

impl Evaluate for IRRef {
    fn evaluate(&self, env: &Environment) -> EvaluationResult {
        match &**self {
            IR::SExpr(exprs) => IR::eval_slist(exprs.as_slice(), env),
            IR::Symbol(name) => env
                .find(name.as_ref())
                .map(|v| (v.clone(), env.clone()))
                .ok_or(anyhow!(format!(
                    "Unable to resolve symbol: {} in this context",
                    name
                ))),
            // self evaluation forms
            _ => Ok((Rc::clone(self).into(), env.clone())),
        }
    }
}

impl From<&Ast> for IRRef {
    fn from(ast: &Ast) -> Self {
        match ast {
            Ast::Integer(v) => Rc::new(IR::Integer(*v)),
            Ast::Symbol(n) => Rc::new(IR::Symbol(String::from(n))),
            Ast::SExpr(xs) => {
                let expr = xs.iter().map(|x| IRRef::from(x)).collect::<Vec<_>>();
                Rc::new(IR::SExpr(expr))
            }
            Ast::QExpr(xs) => {
                let expr = xs.iter().map(|x| IRRef::from(x)).collect::<Vec<_>>();
                Rc::new(IR::QExpr(expr))
            }
        }
    }
}

impl From<IRRef> for Expression {
    fn from(from: IRRef) -> Expression {
        Expression::new(from)
    }
}

impl From<&IRRef> for Expression {
    fn from(from: &IRRef) -> Expression {
        Expression::new(Rc::clone(from))
    }
}

impl From<IR> for Expression {
    fn from(from: IR) -> Expression {
        Expression::from(Rc::new(from))
    }
}
