use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type<Var> {
    Number,
    String,
    Boolean,
    Unit,
    Array(Box<Type<Var>>),
    Tuple(Vec<Type<Var>>),
    Function {
        argument: Box<Type<Var>>,
        result: Box<Type<Var>>,
    },
    Variable(Var),
    // TODO Data(Vec<Type<Var>>),
}

#[derive(Debug)]
pub struct Scheme<Var> {
    pub vars: usize,
    pub ty: Type<SchemeVar<Var>>,
}

#[derive(Debug, Clone)]
pub enum SchemeVar<Var> {
    Free(Var),
    Bound(usize),
}

pub type Substitution<Var> = HashMap<Var, Type<Var>>;

pub trait Free<F> {
    fn free(&self) -> Option<F>;
}

impl<F: Copy> Free<F> for F {
    fn free(&self) -> Option<F> {
        Some(*self)
    }
}

impl<F: Copy> Free<F> for SchemeVar<F> {
    fn free(&self) -> Option<F> {
        match self {
            SchemeVar::Free(x) => Some(*x),
            SchemeVar::Bound(_) => None,
        }
    }
}
