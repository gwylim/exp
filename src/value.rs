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
    Vec {
        values: Vec<Rc<Value<'a>>>,
        vec_type: VecType,
    },
    // Partial application
    Apply {
        function: Function<'a>,
        arity: usize,
        arguments: Vec<Rc<Value<'a>>>,
    },
    // This is used for a recursive reference to a closure
    Recurse,
    Next(Vec<Rc<Value<'a>>>),
}

#[derive(Debug, Clone)]
pub enum Function<'a> {
    Closure {
        body: &'a Expr,
        env: Vec<Rc<Value<'a>>>,
    },
    Builtin(Builtin),
    Constructor {
        type_id: u64,
        index: usize,
    },
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
            Value::Recurse => write!(f, "<recurse>"),
            Value::Apply {
                function: Function::Constructor { type_id, index },
                arity,
                arguments,
            } if *arity == arguments.len() => {
                if arguments.is_empty() {
                    return write!(f, "<constructor {} {}>", type_id, index);
                }
                write!(f, "(<constructor {} {}>", type_id, index)?;
                for v in arguments {
                    write!(f, " ")?;
                    v.fmt(f)?;
                }
                write!(f, ")")
            }
            Value::Apply { .. } => write!(f, "<closure>"),
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
    AppliedNonFunction,
    TypeError,
}

pub type RunResult<'a> = Result<Rc<Value<'a>>, RuntimeError>;
