use crate::value::{Function, RuntimeError, Value};
use std::iter::zip;

pub fn eq<'a>(x: &Value<'a>, y: &Value<'a>) -> Result<bool, RuntimeError> {
    match x {
        Value::Number(n) => {
            if let Value::Number(n1) = y {
                Ok(*n == *n1)
            } else {
                Err(RuntimeError::TypeError)
            }
        }
        Value::Bytes(b) => {
            todo!()
        }
        _ => Err(RuntimeError::TypeError),
    }
}
