use std::io;
use std::io::{stdout, Write};
use crate::parser;
use crate::expression::{ExpressionPool};

fn add_history(_: &str) {}

pub fn do_repl() {
    println!("Lispy version 0.1.0");
    println!("Press Ctrl+c to Exit\n");

    let mut pool = ExpressionPool::new();
    loop {
        print!("lispy> ");
        stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let parse_result = parser::parse(input.trim_end());
                println!("> {:?}", parse_result);
                match parse_result {
                    Ok((_, ast)) => {
                        let e = pool.make(&ast);
                        println!("{:?}", e)
                    },
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
