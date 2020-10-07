extern crate combine;
extern crate itertools;
use combine::{
    attempt,
    between,
    eof,
    error::ParseError,
    many, many1, parser,
    parser::{
        char::{char, digit, letter, space},
        token::one_of,
    },
    satisfy,
    skip_many, skip_many1,
    StdParseResult,
    stream::{Stream, StreamOnce},
    token,
    Parser,
};

/// AST of Lispy
#[derive(Debug)]
pub enum Ast {
    // Atoms
    Integer(i64),
    Symbol(String),
    String_(String),

    // Lists
    SExpr(Vec<Ast>),
    QExpr(Vec<Ast>),
}

impl PartialEq for Ast {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Ast::Integer(i), Ast::Integer(j)) => i == j,
            (Ast::Symbol(s), Ast::Symbol(t)) => s == t,
            (Ast::String_(s), Ast::String_(t)) => s == t,
            (Ast::SExpr(xs), Ast::SExpr(ys)) => xs == ys,
            (Ast::QExpr(xs), Ast::QExpr(ys)) => xs == ys,
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
            Ast::String_(s) => format!("\"{}\"", s),
            Ast::SExpr(xs) => format!(
                "({})",
                itertools::join(xs.iter().map(|x| x.to_string()), " ")
            ),
            Ast::QExpr(xs) => format!(
                "{{{}}}",
                itertools::join(xs.iter().map(|x| x.to_string()), " ")
            ),
        }
    }
}

// Parser definitions
fn comment<Input>() -> impl Parser<Input, Output = ()>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (char(';'), skip_many(satisfy(|c| c != '\n'))).map(|_| ())
}

fn sp<Input>() -> impl Parser<Input, Output = ()>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    skip_many(skip_many1(space()).or(comment()))
}

fn integer<Input>() -> impl Parser<Input, Output = Ast>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    char('-')
        .with(many1::<String, _, _>(digit()).map(|s| Ast::Integer(-s.parse::<i64>().unwrap())))
        .or(many1::<String, _, _>(digit()).map(|s| Ast::Integer(s.parse::<i64>().unwrap())))
}

fn symbol<Input>() -> impl Parser<Input, Output = Ast>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let valid_symbol = || one_of(r"_+-*/\=<>!&?".chars());
    let head = || letter().or(valid_symbol());
    (head(), many::<String, _, _>(head().or(digit())))
        .map(|(h, t)| Ast::Symbol(format!("{}{}", h, t)))
}

fn char_of_string<Input>(input: &mut Input) -> StdParseResult<char, Input>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let (c, committed) = satisfy(|c| c != '"').parse_stream(input).into_result()?;
    match c {
        '\\' => committed.combine(|_| {
            satisfy(|c| c == '"' || c == '\\' || c == 'n' || c == 't')
                .map(|c| {
                    match c {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        't' => '\t',
                        c => c
                    }
                })
                .parse_stream(input)
                .into_result()
        }),
        _ => Ok((c, committed))
    }
}

fn string<Input>() -> impl Parser<Input, Output = Ast>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let element = parser(char_of_string);
    let quoted = many::<String, _, _>(element);
    between(token('"'), token('"'), quoted)
        .map(|s| Ast::String_(s))
}

fn slist<Input>() -> impl Parser<Input, Output = Ast>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    char('(')
        .with(many::<Vec<_>, _, _>(attempt(expr())))
        .skip((sp(), char(')')))
        .map(|es| Ast::SExpr(es))
}

fn qlist<Input>() -> impl Parser<Input, Output = Ast>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    char('{')
        .with(many::<Vec<_>, _, _>(attempt(expr())))
        .skip((sp(), char('}')))
        .map(|es| Ast::QExpr(es))
}

fn _expr<Input>() -> impl Parser<Input, Output = Ast>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    sp().with(symbol().or(integer()).or(string()).or(slist()).or(qlist()))
        .message("expression")
}

parser! {
    fn expr[Input]()(Input) -> Ast
    where [
        Input: Stream<Token = char>,
        Input::Error: ParseError<Input::Token, Input::Range, Input::Position>]
    {
        _expr()
    }
}

pub fn root<Input>() -> impl Parser<Input, Output = Ast>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many::<Vec<_>, _, _>(attempt(expr()))
        .skip(sp())
        .skip(eof())
        .map(|es| Ast::SExpr(es))
}

pub fn root_for_load<Input>() -> impl Parser<Input, Output = Vec<Ast>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many::<Vec<_>, _, _>(attempt(expr())).skip(sp()).skip(eof())
}

pub fn parse<Input>(i: Input) -> Result<(Ast, Input), <Input as StreamOnce>::Error>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let mut parser = root();
    parser.parse(i)
}

#[cfg(test)]
mod tests {
    use super::{sp, root, root_for_load};
    use itertools;
    use combine::EasyParser;
    use combine::stream::position::Stream;

    fn spaces(i: &str) -> String {
        let mut space_parser = sp();
        let stream = Stream::new(i);
        let parse_result = space_parser.easy_parse(stream);
        match parse_result {
            Ok((_, rest)) => rest.input.to_string(),
            Err(e) => e.to_string()
        }
    }

    #[test]
    fn parse_spaces() {
        assert_eq!("", spaces(""));
        assert_eq!("", spaces("        "));
        assert_eq!("hoge", spaces("        hoge"));
        assert_eq!("", spaces(" \n\t\t        "));
        assert_eq!("", spaces("; hoge fuga\n  ; foo bar"));
        assert_eq!("head {1 2 3}", spaces("; hoge fuga\nhead {1 2 3}"));
        assert_eq!("+ 1 2 3  \n", spaces("; hoge fuga\n  ; foo bar\n+ 1 2 3  \n"));
    }

    fn parse_(i: &str) -> String {
        // TODO share an implement with repl::do_repl
        let mut ast_parser = root();
        let stream = Stream::new(i);
        let parse_result = ast_parser.easy_parse(stream);
        match parse_result {
            Ok((ast, _)) => ast.to_string(),
            Err(e) => e.to_string(),
        }
    }

    #[test]
    fn parse_expression() {
        assert_eq!("(+)", parse_("+"));
        assert_eq!(
            "(+ 1 (* 2 3) (- 4 5))",
            parse_("+ 1 (* 2 3) (- 4 5)")
        );
        assert_eq!("((- 100))", parse_("(- 100)"));
        assert_eq!("()", parse_(""));
        assert_eq!("(/)", parse_("/"));
        assert_eq!("({1 2 (+ 5 6) 4})", parse_("{1 2 (+ 5 6) 4}"));
        assert_eq!("(list 1 2 3 4)", parse_("list 1 2 3 4"));
        assert_eq!(
            "(eval {head (list 1 2 3 4)})",
            parse_("eval {head (list 1 2 3 4)}")
        );
        assert_eq!("(def {x} 100)", parse_("def {x} 100"));
        assert_eq!(
            "(def {arglist} {a b x y})",
            parse_("def {arglist} {a b x y}")
        );
        assert_eq!(
            "(def {add-mul} (\\ {x y} {+ x (* x y)}))",
            parse_("def {add-mul} (\\ {x y} {+ x (* x y)})")
        );
        assert_eq!("(> 10 5)", parse_("> 10 5"));
        assert_eq!("(== 5 {})", parse_("== 5 {}"));
        assert_eq!(
            "(== {1 2 3 {5 6}} {1 2 3 {5 6}})",
            parse_("== {1 2 3 {5 6}} {1   2  3   {5 6}}")
        );
        assert_eq!(
            "(if (== x y) {+ x y} {- x y})",
            parse_("if (== x y) {+ x y} {- x y}")
        );
        assert_eq!("(\"hoge\")", parse_("\"hoge\""));
        assert_eq!("(\"aaa\"\n\t\")", parse_("\"aaa\\\"\n\t\""));
        assert_eq!("(error \"fatal error!\")", parse_("error \"fatal error!\""));
        assert_eq!("(+ 1 2 (error \"failed...\"))", parse_("+ 1 2 (error \"failed...\")"));
        assert_eq!("(* 0 1 2)", parse_("; foo bar \n * 0 1 2"));
        assert_eq!("(* 0 1 2)", parse_("; aaa\n* 0 1 ; bbb\n2   "));
        assert_eq!("(+ 1 2 3)", parse_("+ 1 2 3 ; hoge fuga"));
        assert_eq!(
            "(def {fun} (\\ {f b} {def (head f) (\\ (tail f) b)}))",
            parse_("def {fun} (\\ {f b} {\n\
                      def (head f) (\\ (tail f) b)\n\
                    })"))
    }

    fn parse_exprs(c: &str) -> String {
        let mut ast_parser = root_for_load();
        let stream = Stream::new(c);
        let parse_result = ast_parser.easy_parse(stream);
        match parse_result {
            Ok((asts, _)) => itertools::join(asts.iter().map(|ast| ast.to_string()), " "), 
            Err(e) => e.to_string(),
        }
    }

    #[test]
    fn parse_expressions() {
        assert_eq!(
            "(def {add-mul} (\\ {x y} {+ x (* x y)})) (def {add-mul-ten} (add-mul 10))",
            parse_exprs("(def {add-mul} (\\ {x y} {+ x (* x y)}))\n(def {add-mul-ten} (add-mul 10))")
        );
        assert_eq!(
            "(def {true} 1) (def {false} 0)",
            parse_exprs("; true\n\
                         (def {true} 1)\n\
                         ; false\n\
                         (def {false} 0)\n\
                         ; end of file")
        );
        assert_eq!(
            "(def {false} 0) (def {fun} (\\ {f b} {def (head f) (\\ (tail f) b)}))",
            parse_exprs("(def {false} 0)\n\
                         \n\
                         ;;; Functional Functions\n\
                         \n\
                         ; Function Definitions\n\
                         (def {fun} (\\ {f b} {\n\
                         def (head f) (\\ (tail f) b)\n\
                         }))")
        )
    }
}
