extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{digit1, one_of},
    combinator::{cut, map, map_res, all_consuming},
    error::{ParseError, ErrorKind, VerboseError},
    multi::many0,
    sequence::{preceded, terminated, pair},
    IResult
};

#[derive(Debug)]
pub enum Ast {
    // Atoms
    Integer(i64),
    Symbol(String),

    // Lists
    List(Vec<Ast>)
}

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

pub fn parse<'a>(i: &'a str) -> IResult<&'a str, Ast, VerboseError<&'a str>> {
    root(i)
}
