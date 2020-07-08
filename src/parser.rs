extern crate nom;
use nom::{
    IResult
};

pub enum LispyExpression {
    Integer(i64),
    Operator(String)
}

pub fn parse(input: &str) -> IResult<&str, LispyExpression> {
    Ok((input, LispyExpression::Operator("".to_string())))
}
