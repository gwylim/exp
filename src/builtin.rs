use crate::eq::eq;
use crate::value::{RunResult, RuntimeError, Value, VecType};
use std::collections::vec_deque::VecDeque;
use std::io;
use std::rc::Rc;

#[derive(Debug, Copy, Clone)]
pub enum Builtin {
    Print,
    GetLine,
    Length,
    Get,
    Concat,
    Add,
    Sub,
    Mul,
    Div,
    StringLength,
    Substring,
    ConcatString,
    Eq,
    NumberToString,
    Leq,
    And,
    Or,
    Not,
}

// TODO: maybe swap this around, return a word for each Builtin?
pub fn get_builtin(s: &str) -> Option<Builtin> {
    match s {
        "print" => Some(Builtin::Print),
        "get_line" => Some(Builtin::GetLine),
        "length" => Some(Builtin::Length),
        "get" => Some(Builtin::Get),
        "concat" => Some(Builtin::Concat),
        "add" => Some(Builtin::Add),
        "sub" => Some(Builtin::Sub),
        "mul" => Some(Builtin::Mul),
        "div" => Some(Builtin::Div),
        "string_length" => Some(Builtin::StringLength),
        "substring" => Some(Builtin::Substring),
        "concat_string" => Some(Builtin::ConcatString),
        "eq" => Some(Builtin::Eq),
        "number_to_string" => Some(Builtin::NumberToString),
        "leq" => Some(Builtin::Leq),
        "and" => Some(Builtin::And),
        "or" => Some(Builtin::Or),
        "not" => Some(Builtin::Not),
        _ => None,
    }
}

pub fn invoke_builtin(builtin: Builtin, argument_values: VecDeque<Rc<Value>>) -> RunResult {
    match builtin {
        Builtin::Print => invoke_unary(print, argument_values),
        Builtin::Length => invoke_unary(len, argument_values),
        Builtin::Get => invoke_binary(get, argument_values),
        Builtin::Concat => invoke_binary(concat, argument_values),
        Builtin::Add => invoke_binary(add, argument_values),
        Builtin::Sub => invoke_binary(sub, argument_values),
        Builtin::Mul => invoke_binary(mul, argument_values),
        Builtin::Div => invoke_binary(div, argument_values),
        Builtin::Substring => invoke_ternary(substring, argument_values),
        Builtin::ConcatString => invoke_binary(concat_string, argument_values),
        Builtin::GetLine => invoke_nullary(get_line, argument_values),
        Builtin::StringLength => invoke_unary(string_length, argument_values),
        Builtin::Eq => invoke_binary(eq, argument_values),
        Builtin::NumberToString => invoke_unary(number_to_string, argument_values),
        Builtin::Leq => invoke_binary(leq, argument_values),
        Builtin::And => invoke_binary(and, argument_values),
        Builtin::Or => invoke_binary(or, argument_values),
        Builtin::Not => invoke_unary(not, argument_values),
    }
}

trait ArgType<'a> {
    fn from_value<'b>(value: &'b Value<'a>) -> Result<&'b Self, RuntimeError>;
}

impl<'a> ArgType<'a> for Value<'a> {
    fn from_value<'b>(value: &'b Value<'a>) -> Result<&'b Self, RuntimeError> {
        Ok(value)
    }
}

impl<'a> ArgType<'a> for Vec<Rc<Value<'a>>> {
    fn from_value<'b>(value: &'b Value<'a>) -> Result<&'b Self, RuntimeError> {
        match value {
            Value::Vec {
                values,
                vec_type: VecType::Array,
            } => Ok(values),
            _ => Err(RuntimeError::TypeError),
        }
    }
}

impl<'a> ArgType<'a> for String {
    fn from_value<'b>(value: &'b Value<'a>) -> Result<&'b Self, RuntimeError> {
        match &*value {
            Value::String(s) => Ok(s),
            _ => Err(RuntimeError::TypeError),
        }
    }
}

impl<'a> ArgType<'a> for f64 {
    fn from_value<'b>(value: &'b Value<'a>) -> Result<&'b Self, RuntimeError> {
        match &*value {
            Value::Number(x) => Ok(x),
            _ => Err(RuntimeError::TypeError),
        }
    }
}

impl<'a> ArgType<'a> for bool {
    fn from_value<'b>(value: &'b Value<'a>) -> Result<&'b Self, RuntimeError> {
        match &*value {
            Value::Boolean(b) => Ok(b),
            _ => Err(RuntimeError::TypeError),
        }
    }
}
trait ResultType<'a> {
    fn to_result(self) -> RunResult<'a>;
}

impl<'a> ResultType<'a> for Rc<Value<'a>> {
    fn to_result(self) -> RunResult<'a> {
        Ok(self)
    }
}

impl<'a, T: ResultType<'a>> ResultType<'a> for Result<T, RuntimeError> {
    fn to_result(self) -> RunResult<'a> {
        self?.to_result()
    }
}

impl<'a> ResultType<'a> for Value<'a> {
    fn to_result(self) -> RunResult<'a> {
        Ok(Rc::new(self))
    }
}

impl<'a> ResultType<'a> for f64 {
    fn to_result(self) -> RunResult<'a> {
        Ok(Rc::new(Value::Number(self)))
    }
}

impl<'a> ResultType<'a> for String {
    fn to_result(self) -> RunResult<'a> {
        Ok(Rc::new(Value::String(self)))
    }
}

impl<'a> ResultType<'a> for bool {
    fn to_result(self) -> RunResult<'a> {
        Ok(Rc::new(Value::Boolean(self)))
    }
}

fn invoke_nullary<'a, R, F>(f: F, args: VecDeque<Rc<Value<'a>>>) -> RunResult<'a>
where
    R: ResultType<'a>,
    F: Fn() -> R,
{
    if args.len() != 0 {
        return Err(RuntimeError::InvalidNumberOfArguments {
            expected: 0,
            received: args.len(),
        });
    }
    f().to_result()
}

fn invoke_unary<'a, A: 'a, R, F>(f: F, args: VecDeque<Rc<Value<'a>>>) -> RunResult<'a>
where
    A: ArgType<'a>,
    R: ResultType<'a>,
    F: Fn(&A) -> R,
{
    if args.len() != 1 {
        return Err(RuntimeError::InvalidNumberOfArguments {
            expected: 1,
            received: args.len(),
        });
    }
    let arg1 = args.into_iter().next().unwrap();
    f(A::from_value(arg1.as_ref())?).to_result()
}

fn invoke_binary<'a, A: 'a, B: 'a, R, F>(f: F, args: VecDeque<Rc<Value<'a>>>) -> RunResult<'a>
where
    A: ArgType<'a>,
    B: ArgType<'a>,
    R: ResultType<'a>,
    F: Fn(&A, &B) -> R,
{
    if args.len() != 2 {
        return Err(RuntimeError::InvalidNumberOfArguments {
            expected: 2,
            received: args.len(),
        });
    }
    let mut iter = args.into_iter();
    f(
        A::from_value(iter.next().unwrap().as_ref())?,
        B::from_value(iter.next().unwrap().as_ref())?,
    )
    .to_result()
}

fn invoke_ternary<'a, A: 'a, B: 'a, C: 'a, R, F>(
    f: F,
    args: VecDeque<Rc<Value<'a>>>,
) -> RunResult<'a>
where
    A: ArgType<'a>,
    B: ArgType<'a>,
    C: ArgType<'a>,
    R: ResultType<'a>,
    F: Fn(&A, &B, &C) -> R,
{
    if args.len() != 3 {
        return Err(RuntimeError::InvalidNumberOfArguments {
            expected: 3,
            received: args.len(),
        });
    }
    let mut iter = args.into_iter();
    f(
        A::from_value(iter.next().unwrap().as_ref())?,
        B::from_value(iter.next().unwrap().as_ref())?,
        C::from_value(iter.next().unwrap().as_ref())?,
    )
    .to_result()
}

fn get_int(x: f64) -> i64 {
    let result = x as i64;
    if result as f64 != x {
        panic!("Expecting integer");
    }
    result
}

fn print<'a>(s: &String) -> Value<'a> {
    println!("{}", s);
    Value::Unit
}

fn len(vec: &Vec<Rc<Value>>) -> f64 {
    vec.len() as f64
}

fn get<'a>(vec: &Vec<Rc<Value<'a>>>, i: &f64) -> Rc<Value<'a>> {
    let index = get_int(*i);
    if index < 0 || index >= vec.len() as i64 {
        panic!("vector index out of range");
    }
    vec[index as usize].clone()
}

fn concat<'a>(a: &Vec<Rc<Value<'a>>>, b: &Vec<Rc<Value<'a>>>) -> Rc<Value<'a>> {
    let mut values = a.clone();
    values.extend(b.iter().map(|rc| rc.clone()));
    Rc::new(Value::Vec {
        values,
        vec_type: VecType::Array,
    })
}

fn add(x: &f64, y: &f64) -> f64 {
    *x + *y
}

fn sub(x: &f64, y: &f64) -> f64 {
    *x - *y
}

fn mul(x: &f64, y: &f64) -> f64 {
    *x * *y
}

fn div(x: &f64, y: &f64) -> f64 {
    *x / *y
}

fn substring<'a>(s: &String, start: &f64, end: &f64) -> String {
    s[(get_int(*start) as usize)..(get_int(*end) as usize)].to_string()
}

fn concat_string<'a>(s1: &String, s2: &String) -> String {
    s1.to_string() + s2
}

fn get_line<'a>() -> String {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).unwrap();
    buffer
}

fn string_length<'a>(s: &String) -> f64 {
    s.len() as f64
}

fn number_to_string<'a>(x: &f64) -> String {
    x.to_string()
}

fn leq<'a>(x: &f64, y: &f64) -> bool {
    *x <= *y
}

fn and(x: &bool, y: &bool) -> bool {
    *x && *y
}

fn or(x: &bool, y: &bool) -> bool {
    *x || *y
}

fn not(x: &bool) -> bool {
    !*x
}
