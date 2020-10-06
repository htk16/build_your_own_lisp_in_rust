extern crate combine;
use crate::environment::Environment;
use crate::expression::{Evaluate, Expression};
use crate::parser;
use combine::EasyParser;
use combine::stream::position::Stream;
use std::io;
use std::io::{stdout, Write};

fn add_history(_: &str) {}

pub fn do_repl() {
    println!("Lispy version 0.11.0");
    println!("Press Ctrl+c to Exit\n");

    let mut env = Environment::init();

    loop {
        print!("lispy> ");
        stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                println!("\"{}\"", input);
                let stream = Stream::new(input.as_str());
                let mut ast_parser = parser::root();
                let parse_result = ast_parser.easy_parse(stream);
                println!("> {:?}", parse_result);
                match parse_result {
                    Ok((ast, _)) => {
                        let expr = Expression::from(&ast);
                        let evaluated_result = expr.evaluate(&env);
                        match evaluated_result {
                            Ok((expr, new_env)) => {
                                println!("{}", expr.to_string());
                                env = new_env
                            }
                            Err(msg) => println!("Error: {}", msg),
                        }
                    }
                    Err(e) => println!("parse error: {}", e.to_string()),
                };
                add_history(&input);
            }
            Err(error) => {
                println!("unexpected error: {}", error);
                break;
            }
        };
    }
}
