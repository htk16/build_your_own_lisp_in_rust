use std::io;
use std::io::{stdout, Write};
use crate::parser;

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
                let e = parser::parse(input.trim_end());
                println!("> {:?}", e);
                add_history(&input);
            }
            Err(error) => {
                println!("unexpected error: {}", error);
                break;
            }
        };
    }
}
