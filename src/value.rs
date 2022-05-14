use crate::builtin::Builtin;
use crate::eq::eq;
use crate::Expr;
use regex::Regex;
use std::borrow::Cow;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Number(f64),
    String(String),
    Boolean(bool),
    Unit,
    Builtin(Builtin),
    Vec(Vec<Rc<Value<'a>>>),
    Closure {
        arity: usize,
        body: &'a Expr,
        env: Vec<Rc<Value<'a>>>,
    },
    // This is used for a recursive reference to a closure
    Recurse,
    Label(u64),
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
            Value::String(s) => write!(f, "\"[{}]", escape_string(s)),
            Value::Boolean(b) => b.fmt(f),
            Value::Unit => write!(f, "()"),
            Value::Builtin(_b) => write!(f, "<builtin function>"),
            Value::Vec(vec) => {
                write!(f, "#(")?;
                for i in 0..vec.len() {
                    vec[i].fmt(f)?;
                    if i < vec.len() - 1 {
                        " ".fmt(f)?;
                    }
                }
                write!(f, ")")?;
                Ok(())
            }
            Value::Closure { .. } => write!(f, "<closure>"),
            Value::Recurse => write!(f, "<recurse>"),
            Value::Label(i) => write!(f, "<label {}>", i),
        }
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    MultiplePatternsMatched,
    NoPatternsMatched,
    InvalidNumberOfArguments { expected: usize, received: usize },
    AppliedNonFunction,
    TypeError,
}

pub type RunResult<'a> = Result<Rc<Value<'a>>, RuntimeError>;
