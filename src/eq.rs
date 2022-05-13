use crate::value::{RuntimeError, Value};
use std::cmp::min;

pub fn eq<'a>(x: &Value<'a>, y: &Value<'a>) -> Result<bool, RuntimeError> {
    match x {
        Value::Number(n) => {
            if let Value::Number(n1) = y {
                Ok(*n == *n1)
            } else {
                Err(RuntimeError::TypeError)
            }
        }
        Value::String(s) => {
            if let Value::String(s1) = y {
                Ok(s.eq(s1))
            } else {
                Err(RuntimeError::TypeError)
            }
        }
        Value::Boolean(b) => {
            if let Value::Boolean(b1) = y {
                Ok(*b == *b1)
            } else {
                Err(RuntimeError::TypeError)
            }
        }
        Value::Vec(vec) => {
            if let Value::Vec(vec1) = y {
                for i in 0..min(vec.len(), vec1.len()) {
                    if !eq(&vec[i], &vec1[i])? {
                        return Ok(false);
                    }
                }
                if vec.len() != vec1.len() {
                    return Err(RuntimeError::TypeError);
                }
                Ok(true)
            } else {
                Err(RuntimeError::TypeError)
            }
        }
        Value::Unit => {
            if let Value::Unit = *y {
                Ok(true)
            } else {
                Err(RuntimeError::TypeError)
            }
        }
        Value::Label(n) => {
            if let Value::Label(n1) = y {
                Ok(*n == *n1)
            } else {
                Err(RuntimeError::TypeError)
            }
        }
        _ => Err(RuntimeError::TypeError),
    }
}
