extern crate nom;
use crate::expression::Expression;
use crate::parser;
use nom::error::ErrorKind;
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
                let parse_result = parser::parse::<(&str, ErrorKind)>(input.trim_end());
                println!("> {:?}", parse_result);
                match parse_result {
                    Ok((_, ast)) => {
                        let expr = Expression::from(&ast);
                        println!("{:?}", expr);
                        let evalated_expr = expr.evaluate();
                        println!("{:?}", evalated_expr);
                    }
                    Err(error) => {
                        println!("parse error: {}", error);
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
