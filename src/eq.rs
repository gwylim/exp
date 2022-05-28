use crate::value::{Function, RuntimeError, Value, VecType};
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
        Value::Vec { values, vec_type } => {
            if let Value::Vec {
                values: values1,
                vec_type: vec_type1,
            } = y
            {
                if vec_type != vec_type1 {
                    return Err(RuntimeError::TypeError);
                }
                if values.len() != values1.len() {
                    return match vec_type {
                        VecType::Array => Ok(false),
                        VecType::Tuple => Err(RuntimeError::TypeError),
                    };
                }
                let mut matched = true;
                for (x, y) in zip(values, values1) {
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
        Value::Apply {
            function: Function::Constructor { type_id, index },
            arity,
            arguments,
        } if *arity == arguments.len() => {
            if let Value::Apply {
                function:
                    Function::Constructor {
                        type_id: type_id1,
                        index: index1,
                    },
                arity: arity1,
                arguments: arguments1,
            } = y
            {
                if *arity1 != arguments1.len() {
                    return Err(RuntimeError::TypeError);
                }
                if type_id != type_id1 {
                    return Err(RuntimeError::TypeError);
                }
                if index != index1 {
                    return Ok(false);
                }
                let mut matched = true;
                for (v, v1) in zip(arguments, arguments1) {
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
