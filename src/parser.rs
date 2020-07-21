extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{digit1, one_of},
    combinator::{cut, map, map_res},
    error::{ParseError, ErrorKind, VerboseError},
    multi::many0,
    sequence::{preceded, terminated, pair},
    IResult,
};

#[derive(Debug)]
pub enum Atom {
    Integer(i64),
    Operator(String)
}

#[derive(Debug)]
pub enum Expression {
    Constant(Box<Atom>),
    Application(Box<Expression>, Vec<Expression>)
}

fn sp<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}

fn integer<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Atom, E> {
    let pos_integer = map_res(
        digit1,
        |digit_str: &str| {digit_str.parse::<i64>().map(Atom::Integer)});
    let neg_integer = map_res(
        preceded(tag("-"), digit1),
        |digit_str: &str| {digit_str.parse::<i64>().map(Atom::Integer)});
    alt((pos_integer, neg_integer))(i)
}

fn operator<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Atom, E> {
    map(one_of("+-*/"), |op| { Atom::Operator(op.to_string()) })(i)
}

fn constant<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Expression, E> {
    map(alt((integer, operator)), |a| {Expression::Constant(Box::new(a))})(i)
}

fn application<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Expression, E> {
    let _operator = preceded(sp, map(operator, |a| {Expression::Constant(Box::new(a))}));
    map(
        pair(_operator, many0(expr)),
        |(op, exprs)| { Expression::Application(Box::new(op), exprs) }
    )(i)
}

pub fn expr<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Expression, E> {
    preceded(
        sp,
        alt((application, constant))
    )(i)
}

pub fn root<'a>(i: &'a str) -> IResult<&'a str, Expression, VerboseError<&'a str>> {
    expr(i)
}
