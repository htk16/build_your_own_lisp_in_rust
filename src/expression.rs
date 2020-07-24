use std::rc::Rc;
use crate::parser::Ast;

#[derive(Debug)]
pub enum Expression {
    // Atoms
    Nil,
    Integer(i64),
    Symbol(String),

    // Lists
    Cons(ExpressionRef, ExpressionRef)
}

pub type ExpressionRef = Rc<Expression>;

pub struct ExpressionPool {
    expressions: Vec<ExpressionRef>
}

impl ExpressionPool {
    pub fn new() -> ExpressionPool {
        ExpressionPool{expressions: vec!()}
    }

    pub fn make(&mut self, ast: &Ast) -> ExpressionRef {
        let expr = self.make_expression(ast);
        self.register(expr)
    }

    fn register(&mut self, expr: Expression) -> ExpressionRef {
        let expr_ref = Rc::new(expr);
        let result = Rc::clone(&expr_ref);
        self.expressions.push(expr_ref);
        result
    }

    fn make_expression(&mut self, ast: &Ast) -> Expression {
        match ast {
            Ast::Integer(v) => Expression::Integer(*v),
            Ast::Symbol(n) => Expression::Symbol(n.to_string()),
            Ast::List(xs) => xs.iter()
                .rev()
                .fold(Expression::Nil, |acc, x| Expression::Cons(self.make(&x), self.register(acc))),
        }
    }
}
