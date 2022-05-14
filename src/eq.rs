use crate::value::{RuntimeError, Value};
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
                if vec.len() != vec1.len() {
                    return Err(RuntimeError::TypeError);
                }
                let mut matched = true;
                for (x, y) in zip(vec, vec1) {
                    if !eq(x, y)? {
                        matched = false;
                    }
                }
                Ok(matched)
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
        Value::Constructed {
            type_id,
            index,
            values,
        } => {
            if let Value::Constructed {
                type_id: type_id1,
                index: index1,
                values: values1,
            } = y
            {
                if type_id != type_id1 {
                    return Err(RuntimeError::TypeError);
                }
                if index != index1 {
                    return Ok(false);
                }
                let mut matched = true;
                for (v, v1) in zip(values, values1) {
                    if !eq(v, v1)? {
                        matched = false;
                    }
                }
                Ok(matched)
            } else {
                Err(RuntimeError::TypeError)
            }
        }
        _ => Err(RuntimeError::TypeError),
    }
}
