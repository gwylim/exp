use crate::builtin::Builtin;
use crate::eq::eq;
use crate::Expr;
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

#[derive(Debug)]
pub enum RuntimeError {
    MultiplePatternsMatched,
    NoPatternsMatched,
    InvalidNumberOfArguments { expected: usize, received: usize },
    AppliedNonFunction,
    TypeError,
}

pub type RunResult<'a> = Result<Rc<Value<'a>>, RuntimeError>;
