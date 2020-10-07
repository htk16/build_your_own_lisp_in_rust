extern crate combine;
extern crate structopt;
use crate::environment::Environment;
use crate::expression::{Evaluate, Expression};
use crate::parser;
use combine::EasyParser;
use combine::stream::position::Stream;
use structopt::StructOpt;
use anyhow::anyhow;
use std::path::PathBuf;
use std::io;
use std::io::{stdout, Write};
use std::fs;

/// Lispy interpreter
#[derive(StructOpt, Debug)]
#[structopt(name = "lispy")]
struct Opt {
    /// Files to execute
    #[structopt(name = "FILE", parse(from_os_str))]
    files: Vec<PathBuf>
}

pub fn run_lispy() -> anyhow::Result<()> {
    let opt = Opt::from_args();

    if opt.files.len() == 0 {
        // Execute REPL
        run_repl()
    } else {
        // Execute scripts
        run_scripts(&opt.files)
    }
}

fn add_history(_: &str) {}

pub fn run_repl() -> anyhow::Result<()>{
    println!("Lispy version 0.14.0");
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

    Ok(())
}

pub fn run_scripts(files: &Vec<PathBuf>) -> anyhow::Result<()> {
    let mut env = Environment::init();
    for file in files {
        let contents = fs::read_to_string(file)?;
        let stream = Stream::new(contents.as_str());
        let mut file_parser = parser::root_for_load();
        let parse_result = file_parser.easy_parse(stream);
        match parse_result {
            Ok((asts, _)) => {
                let exprs = asts.iter().map(|ast| Expression::from(ast));
                for expr in exprs {
                    match expr.evaluate(&env) {
                        Ok((_, new_env)) => env = new_env,
                        Err(msg) => return Err(anyhow!("Error: {}", msg))
                    }
                }
            },
            Err(parse_error) => {
                return Err(anyhow!("Error: {}", parse_error))
            }
        }
    };
    Ok(())
}
