extern crate combine;
use crate::expression::{Evaluate, Expression};
use crate::parser;
use combine::error::StringStreamError;
use std::io;
use std::io::{stdout, Write};

fn add_history(_: &str) {}

pub fn do_repl() {
    println!("Lispy version 0.1.0");
    println!("Press Ctrl+c to Exit\n");

    loop {
        print!("lispy> ");
        stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let parse_result = parser::parse(input.trim_end());
                println!("> {:?}", parse_result);
                match parse_result {
                    Ok((ast, _)) => {
                        let expr = Expression::from(&ast);
                        let evaluated_result = expr.evaluate();
                        match evaluated_result {
                            Ok(expr) => println!("{}", expr.to_string()),
                            Err(msg) => println!("Error: {}", msg)
                        }
                    }
                    Err(e) => {
                        println!("parse error: {:?}", e)
                    }
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
