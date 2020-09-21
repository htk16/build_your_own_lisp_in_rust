use anyhow::{anyhow, Result};
use crate::expression::{Expression, ExpressionType};
use crate::error;

pub type ValidationResult = Result<()>;

pub trait ArgumentsValidation {
    fn required(&self, required: usize) -> ValidationResult;
    fn required_more_than_or_equal(&self, required: usize) -> ValidationResult;
}

impl<'a, 'b> ArgumentsValidation for (&'a str, &'b [Expression]) {
    fn required(&self, required: usize) -> ValidationResult {
        if self.1.len() != required {Err(error::argument_error(self.0, required, self.1.len()))} else {Ok(())}
    }

    fn required_more_than_or_equal(&self, required: usize) -> ValidationResult {
        if self.1.len() < required {
            Err(anyhow!("Function '{}' required {} or more argument(s), but passed {}", self.0, required, self.1.len()))
        } else {
            Ok(())
        }
    }
}

pub trait TypeValidation {
    fn required_type(&self, et: ExpressionType) -> ValidationResult;
}

impl TypeValidation for &Expression {
    fn required_type(&self, required: ExpressionType) -> ValidationResult {
        self.type_().required_type(required)
    }
}

impl TypeValidation for ExpressionType {
    fn required_type(&self, required: ExpressionType) -> ValidationResult {
        if *self != required {
            Err(error::type_error(required, *self))
        } else {
            Ok(())
        }
    }
}
