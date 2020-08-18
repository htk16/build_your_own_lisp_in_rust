extern crate itertools;
extern crate combine;
use combine::{
    parser::{
        char::{char, digit, letter, space},
        token::one_of,
    },
    error::ParseError,
    stream::{Stream, StreamOnce, easy},
    Parser, EasyParser, parser, many, many1, skip_many
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
fn sp<Input>() -> impl Parser<Input, Output=()>
where Input: Stream<Token = char>,
      Input::Error: ParseError<Input::Token, Input::Range, Input::Position>
{
    skip_many(space())
}

fn integer<Input>() -> impl Parser<Input, Output=Ast>
where Input: Stream<Token = char>,
      Input::Error: ParseError<Input::Token, Input::Range, Input::Position>
{
    char('-')
        .with(many1::<String, _, _>(digit()).map(|s| Ast::Integer(-s.parse::<i64>().unwrap())))
        .or(many1::<String, _, _>(digit()).map(|s| Ast::Integer(s.parse::<i64>().unwrap())))
}

fn symbol<Input>() -> impl Parser<Input, Output=Ast>
where Input: Stream<Token = char>,
      Input::Error: ParseError<Input::Token, Input::Range, Input::Position>
{
    let valid_symbol = || one_of(r"+-*/\=<>!&?".chars());
    let head = || letter().or(valid_symbol());
    (head(), many::<String, _, _>(head().or(digit())))
        .map(|(h, t)| Ast::Symbol(format!("{}{}", h, t)))
}

fn slist<Input>() -> impl Parser<Input, Output=Ast>
where Input: Stream<Token = char>,
      Input::Error: ParseError<Input::Token, Input::Range, Input::Position>
{
    char('(')
        .with(many::<Vec<_>, _, _>(expr()))
        .skip((sp(), char(')')))
        .map(|es| Ast::SExpr(es))
}

fn qlist<Input>() -> impl Parser<Input, Output=Ast>
where Input: Stream<Token = char>,
      Input::Error: ParseError<Input::Token, Input::Range, Input::Position>
{
    char('{')
        .with(many::<Vec<_>, _, _>(expr()))
        .skip((sp(), char('}')))
        .map(|es| Ast::QExpr(es))
}

fn _expr<Input>() -> impl Parser<Input, Output=Ast>
where Input: Stream<Token = char>,
      Input::Error: ParseError<Input::Token, Input::Range, Input::Position>
{
    sp().with(symbol()
              .or(integer())
              .or(slist())
              .or(qlist()))
}

parser!{
    fn expr[Input]()(Input) -> Ast
    where [
        Input: Stream<Token = char>,
        Input::Error: ParseError<Input::Token, Input::Range, Input::Position>]
    {
        _expr()
    }
}

pub fn root<Input>() -> impl Parser<Input, Output=Ast>
where Input: Stream<Token = char>,
      Input::Error: ParseError<Input::Token, Input::Range, Input::Position>
{
    many::<Vec<_>, _, _>(expr())
        .map(|es| Ast::SExpr(es))
}

pub fn parse<Input>(i: Input) -> Result<(Ast, Input), <Input as StreamOnce>::Error>
where Input: Stream<Token = char>,
      Input::Error: ParseError<Input::Token, Input::Range, Input::Position>
{
    let mut parser = root();
    parser.parse(i)
}

// pub fn easy_parse<'a, Input>(i: Input) -> Result<(Ast, Input), easy::ParseError<Input>>
// where Input: Stream<Token = char, Range = &'a str>,
//       easy::Stream<Input>: StreamOnce<Token = Input::Token, Range = Input::Range, Error = easy::ParseError<easy::Stream<Input>>, Position = Input::Position>,
//       Input::Position: Default,
// {
//     let mut parser = root();
//     parser.easy_parse(i)
// }

#[cfg(test)]
mod tests {
    use super::root;
    use crate::parser::combine::Parser;

    fn parse_and_format(i: &str) -> String {
        let mut parser = root();
        match parser.parse(i) {
            Ok((ast, _)) => ast.to_string(),
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
