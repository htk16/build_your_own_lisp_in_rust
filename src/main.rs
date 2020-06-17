use std::io;
use std::io::{stdout, Write};

fn add_history(_: &str) {}

fn main() {
    println!("Lispy version 0.1.0");
    println!("Press Ctrl+c to Exit\n");

    loop {
        print!("lispy> ");
        stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                println!("No you're a {}", input);
                add_history(&input);
            }
            Err(error) => {
                println!("unexpected error: {}", error);
                break;
            }
        };
    }
}
