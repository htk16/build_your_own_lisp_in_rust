use anyhow::{anyhow, Error};

pub fn make_argument_error(name: &str, required: usize, passed: usize) -> Error {
    anyhow!("Function '{}' required {} argument(s), but passed {}", name, required, passed)
}

pub fn make_type_error<'a, 'b>(required: &'a str, passed: &'b str) -> Error {
    anyhow!("{} required, but passed {}", required, passed)
}

