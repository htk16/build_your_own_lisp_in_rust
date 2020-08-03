extern crate nom;
extern crate itertools;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{digit1, one_of},
    combinator::{map, map_res, all_consuming},
    error::ParseError,
    multi::many0,
    sequence::{preceded, terminated, pair},
    IResult
};

/// AST of Rispy
#[derive(Debug)]
pub enum Ast {
    // Atoms
    Integer(i64),
    Symbol(String),

    // Lists
    List(Vec<Ast>)
}

impl PartialEq for Ast {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Ast::Integer(i), Ast::Integer(j)) => i == j,
            (Ast::Symbol(s), Ast::Symbol(t)) => s == t,
            (Ast::List(xs), Ast::List(ys)) => xs == ys,
            _ => false
        }
    }
}

impl Eq for Ast {}

impl ToString for Ast {
    fn to_string(&self) -> String {
        match self {
            Ast::Integer(i) => i.to_string(),
            Ast::Symbol(s) => s.clone(),
            Ast::List(xs) => format!("({})", itertools::join(xs.iter().map(|x| x.to_string()), " "))
        }
    }
}

// Parser definitions
fn sp<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}

fn integer<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    let pos_integer = map_res(
        digit1,
        |digit_str: &str| {digit_str.parse::<i64>().map(Ast::Integer)});
    let neg_integer = map_res(
        preceded(tag("-"), digit1),
        |digit_str: &str| {digit_str.parse::<i64>().map(Ast::Integer)});
    alt((pos_integer, neg_integer))(i)
}

fn operator<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    map(one_of("+-*/"), |op| { Ast::Symbol(op.to_string()) })(i)
}

fn constant<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    alt((integer, operator))(i)
}

fn application<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    let _application = preceded(
        tag("("),
        terminated(
            pair(operator, many0(expr)),
            pair(sp, tag(")"))
        )
    );
    map(_application, |(op, mut exprs)| {
        let mut elems = vec!(op);
        elems.append(&mut exprs);
        Ast::List(elems)
    })(i)
}

fn expr<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    preceded(
        sp,
        alt((application, constant))
    )(i)
}

fn root<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    let _root = map(
        pair(operator, many0(expr)),
        |(op, mut exprs)| {
            let mut elems = vec!(op);
            elems.append(&mut exprs);
            Ast::List(elems)
        }
    );
    all_consuming(_root)(i)
}

pub fn parse<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    root(i)
}

#[cfg(test)]
mod tests {
    use super::parse;
    use nom::error::ErrorKind;

    fn parse_and_pprint(i: &str) -> String {
        match parse::<(&str, ErrorKind)>(i) {
            Ok((_, ast)) => ast.to_string(),
            Err(e) => e.to_string()
        }
    }

    #[test]
    fn parse_expression() {
        assert_eq!("(+)".to_string(), parse_and_pprint("+"));
        assert_eq!("(+ 1 (* 2 3) (- 4 5))".to_string(), parse_and_pprint("+ 1 (* 2 3) (- 4 5)"));
        // assert_eq!("".to_string(), parse_and_pprint("hoge fuga") )
    }
}
