extern crate itertools;
extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::digit1,
    combinator::{all_consuming, map, map_res},
    error::{context, ParseError},
    multi::many0,
    sequence::{pair, preceded, terminated},
    IResult,
};

/// AST of Rispy
#[derive(Debug)]
pub enum Ast {
    // Atoms
    Integer(i64),
    Symbol(String),

    // Lists
    SExpr(Vec<Ast>),
    QExpr(Vec<Ast>)
}

impl PartialEq for Ast {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Ast::Integer(i), Ast::Integer(j)) => i == j,
            (Ast::Symbol(s), Ast::Symbol(t)) => s == t,
            (Ast::SExpr(xs), Ast::SExpr(ys)) => xs == ys,
            _ => false,
        }
    }
}

impl Eq for Ast {}

impl ToString for Ast {
    fn to_string(&self) -> String {
        match self {
            Ast::Integer(i) => i.to_string(),
            Ast::Symbol(s) => s.clone(),
            Ast::SExpr(xs) => format!(
                "({})",
                itertools::join(xs.iter().map(|x| x.to_string()), " ")
            ),
            Ast::QExpr(xs) => format!(
                "{{{}}}",
                itertools::join(xs.iter().map(|x| x.to_string()), " ")
            )
        }
    }
}

// Parser definitions
fn sp<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}

fn integer<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    let pos_integer = map_res(digit1, |digit_str: &str| {
        digit_str.parse::<i64>().map(Ast::Integer)
    });
    let neg_integer = map_res(preceded(tag("-"), digit1), |digit_str: &str| {
        digit_str.parse::<i64>().map(Ast::Integer)
    });
    context("integer", alt((pos_integer, neg_integer)))(i)
}

fn symbol<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    context("symbol",
            map(alt((
                tag("+"),
                tag("-"),
                tag("*"),
                tag("/"),
                tag("list"),
                tag("head"),
                tag("tail"),
                tag("join"),
                tag("eval")
            )), |sym: &str| Ast::Symbol(sym.to_string())))(i)
}

fn slist<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    let _application = preceded(
        tag("("),
        terminated(many0(expr), pair(sp, tag(")"))),
    );
    map(_application, |exprs| { Ast::SExpr(exprs) })(i)
}

fn qlist<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    let _qlist = preceded(
        tag("{"),
        terminated(many0(expr), pair(sp, tag("}"))),
    );
    map(_qlist, |exprs| { Ast::QExpr(exprs)} )(i)
}

fn expr<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    context("expr", preceded(sp, alt((integer, symbol, slist, qlist))))(i)
}

fn root<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    let _root = map(many0(expr), |exprs| { Ast::SExpr(exprs) });
    all_consuming(_root)(i)
}

pub fn parse<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    root(i)
}

#[cfg(test)]
mod tests {
    use super::parse;
    use nom::error::ErrorKind;

    fn parse_and_format(i: &str) -> String {
        match parse::<(&str, ErrorKind)>(i) {
            Ok((_, ast)) => ast.to_string(),
            Err(e) => e.to_string(),
        }
    }

    #[test]
    fn parse_expression() {
        assert_eq!("(+)", parse_and_format("+"));
        assert_eq!("(+ 1 (* 2 3) (- 4 5))", parse_and_format("+ 1 (* 2 3) (- 4 5)"));
        assert_eq!("((- 100))", parse_and_format("(- 100)"));
        assert_eq!("()", parse_and_format(""));
        assert_eq!("(/)", parse_and_format("/"));
        assert_eq!("({1 2 (+ 5 6) 4})", parse_and_format("{1 2 (+ 5 6) 4}"));
        assert_eq!("(list 1 2 3 4)", parse_and_format("list 1 2 3 4"));
        assert_eq!("(eval {head (list 1 2 3 4)})", parse_and_format("eval {head (list 1 2 3 4)}"))
    }
}
