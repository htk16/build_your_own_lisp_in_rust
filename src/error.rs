use anyhow::{anyhow, Error};
use crate::expression::{Expression, ExpressionType};

pub fn argument_error(name: &str, required: usize, passed: usize) -> Error {
    anyhow!("Function '{}' required {} argument(s), but passed {}", name, required, passed)
}

pub fn type_error(required: ExpressionType, passed: ExpressionType) -> Error {
    anyhow!("{} required, but passed {}", required.name(), passed.name())
}

pub fn expression_type_error(required: ExpressionType, passed: &Expression) -> Error {
    anyhow!("{} required, but passed {} ({})", required.name(), passed.type_name(), passed.to_string())
}

pub fn divide_by_zero() -> Error {
    anyhow!("Divide by zero")
}

pub fn fatal_error() -> Error {
    anyhow!("Fatal error!")
}
