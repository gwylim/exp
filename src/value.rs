use crate::builtin::Builtin;
use crate::eq::eq;
use crate::expr::Expr;
use crate::located::Located;
use regex::Regex;
use std::borrow::Cow;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Nil,
    Symbol(String),
    Cons(Box<Value<'a>>, Box<Value<'a>>),
    Bytes(Vec<u8>),
    Number(i64),
    // Partial application
    Apply {
        function: Function<'a>,
        arity: usize,
        arguments: Vec<Rc<Value<'a>>>,
    },
    // This is used for a recursive reference to a closure
    Recurse,
}

#[derive(Debug, Clone)]
pub struct Binding<'a> {
    name: String,
    value: Rc<Value<'a>>,
}

#[derive(Debug, Clone)]
pub enum Function<'a> {
    Closure {
        body: &'a Expr,
        env: Vec<Binding<'a>>,
    },
    Builtin(Builtin),
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
            Value::Bytes(bytes) => {
                write!(f, "0x")?;
                for byte in bytes {
                    write!(f, "{:02x}", byte)?;
                }
                Ok(())
            }
            Value::Number(n) => n.fmt(f),
            Value::Recurse => write!(f, "<recurse>"),
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
pub enum SyntaxError {
    InvalidNumericLiteral,
    InvalidBytesLiteral,
}

#[derive(Debug, PartialEq, Eq)]
pub enum RuntimeError {
    SyntaxError(SyntaxError),
    TypeError,
}

pub type EvalResult<'a> = Result<Rc<Value<'a>>, Located<RuntimeError>>;
