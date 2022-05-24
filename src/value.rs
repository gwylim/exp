use crate::builtin::Builtin;
use crate::eq::eq;
use crate::expr::Expr;
use regex::Regex;
use std::borrow::Cow;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VecType {
    Array,
    Tuple,
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Number(f64),
    String(String),
    Boolean(bool),
    Unit,
    Builtin(Builtin),
    Vec {
        values: Vec<Rc<Value<'a>>>,
        vec_type: VecType,
    },
    Closure {
        arity: usize,
        body: &'a Expr,
        env: Vec<Rc<Value<'a>>>,
    },
    // This is used for a recursive reference to a closure
    Recurse,
    Constructor {
        type_id: u64,
        index: usize,
        arity: usize,
    },
    Constructed {
        type_id: u64,
        index: usize,
        values: Vec<Rc<Value<'a>>>,
    },
    Next(Vec<Rc<Value<'a>>>),
}

impl<'a> PartialEq for Value<'a> {
    fn eq(&self, other: &Self) -> bool {
        eq(self, other).unwrap()
    }
}

fn escape_string(s: &str) -> Cow<'_, str> {
    Regex::new("[\\\\|\\]]").unwrap().replace_all(s, "\\$0")
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => n.fmt(f),
            Value::String(s) => write!(f, "[\"{}]", escape_string(s)),
            Value::Boolean(b) => b.fmt(f),
            Value::Unit => f.write_str("unit"),
            Value::Builtin(_b) => write!(f, "<builtin function>"),
            Value::Vec {
                values: vec,
                vec_type,
            } => {
                write!(
                    f,
                    "({} ",
                    match vec_type {
                        VecType::Array => "@",
                        VecType::Tuple => "#",
                    }
                )?;
                for i in 0..vec.len() {
                    vec[i].fmt(f)?;
                    if i < vec.len() - 1 {
                        " ".fmt(f)?;
                    }
                }
                write!(f, ")")
            }
            Value::Closure { .. } => write!(f, "<closure>"),
            Value::Recurse => write!(f, "<recurse>"),
            Value::Constructor {
                type_id,
                index,
                arity: _,
            } => write!(f, "<constructor {} {}>", type_id, index),
            Value::Constructed {
                type_id,
                index,
                values,
            } => {
                if values.is_empty() {
                    return write!(f, "<constructed {} {}>", type_id, index);
                }
                write!(f, "(<constructed {} {}>", type_id, index)?;
                for v in values {
                    write!(f, " ")?;
                    v.fmt(f)?;
                }
                write!(f, ")")
            }
            Value::Next(arguments) => {
                f.write_str("(<next>")?;
                for arg in arguments {
                    write!(f, " {}", arg)?;
                }
                f.write_str(")")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum RuntimeError {
    InvalidConstructorPattern,
    MultiplePatternsMatched,
    NoPatternsMatched,
    InvalidNumberOfArguments { expected: usize, received: usize },
    AppliedNonFunction,
    TypeError,
}

pub type RunResult<'a> = Result<Rc<Value<'a>>, RuntimeError>;
