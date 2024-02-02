use crate::eq::eq;
use crate::value::{EvalResult, RuntimeError, Value, VecType};
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
    Mod,
    StringLength,
    Substring,
    ConcatString,
    Eq,
    NumberToString,
    Leq,
    And,
    Or,
    Not,
    ConcatBytes,
    NumberToBytes,
    ShiftRight,
    BinaryAnd,
    BinaryOr,
    BytesLength,
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
        "mod" => Some(Builtin::Mod),
        "string_length" => Some(Builtin::StringLength),
        "substring" => Some(Builtin::Substring),
        "concat_string" => Some(Builtin::ConcatString),
        "eq" => Some(Builtin::Eq),
        "number_to_string" => Some(Builtin::NumberToString),
        "leq" => Some(Builtin::Leq),
        "and" => Some(Builtin::And),
        "or" => Some(Builtin::Or),
        "not" => Some(Builtin::Not),
        "concat_bytes" => Some(Builtin::ConcatBytes),
        "number_to_bytes" => Some(Builtin::NumberToBytes),
        "shift_right" => Some(Builtin::ShiftRight),
        "binary_and" => Some(Builtin::BinaryAnd),
        "binary_or" => Some(Builtin::BinaryOr),
        "bytes_length" => Some(Builtin::BytesLength),
        _ => None,
    }
}

pub fn invoke_builtin(builtin: Builtin, argument_values: VecDeque<Rc<Value>>) -> EvalResult {
    match builtin {
        Builtin::Print => invoke_unary(print, argument_values),
        Builtin::Length => invoke_unary(len, argument_values),
        Builtin::Get => invoke_binary(get, argument_values),
        Builtin::Concat => invoke_binary(concat, argument_values),
        Builtin::Add => invoke_binary(add, argument_values),
        Builtin::Sub => invoke_binary(sub, argument_values),
        Builtin::Mul => invoke_binary(mul, argument_values),
        Builtin::Div => invoke_binary(div, argument_values),
        Builtin::Mod => invoke_binary(mod_function, argument_values),
        Builtin::Substring => invoke_ternary(substring, argument_values),
        Builtin::ConcatString => invoke_binary(concat_string, argument_values),
        Builtin::GetLine => invoke_unary(get_line, argument_values),
        Builtin::StringLength => invoke_unary(string_length, argument_values),
        Builtin::Eq => invoke_binary(eq, argument_values),
        Builtin::NumberToString => invoke_unary(number_to_string, argument_values),
        Builtin::Leq => invoke_binary(leq, argument_values),
        Builtin::And => invoke_binary(and, argument_values),
        Builtin::Or => invoke_binary(or, argument_values),
        Builtin::Not => invoke_unary(not, argument_values),
        Builtin::ConcatBytes => invoke_binary(concat_bytes, argument_values),
        Builtin::NumberToBytes => invoke_unary(number_to_bytes, argument_values),
        Builtin::ShiftRight => invoke_binary(shift_right, argument_values),
        Builtin::BinaryAnd => invoke_binary(binary_and, argument_values),
        Builtin::BinaryOr => invoke_binary(binary_or, argument_values),
        Builtin::BytesLength => invoke_unary(bytes_length, argument_values),
    }
}

pub fn arity(builtin: Builtin) -> usize {
    match builtin {
        Builtin::Print => 1,
        Builtin::GetLine => 1,
        Builtin::Length => 1,
        Builtin::Get => 2,
        Builtin::Concat => 2,
        Builtin::Add => 2,
        Builtin::Sub => 2,
        Builtin::Mul => 2,
        Builtin::Div => 2,
        Builtin::Mod => 2,
        Builtin::StringLength => 1,
        Builtin::Substring => 3,
        Builtin::ConcatString => 2,
        Builtin::Eq => 2,
        Builtin::NumberToString => 1,
        Builtin::Leq => 2,
        Builtin::And => 2,
        Builtin::Or => 2,
        Builtin::Not => 2,
        Builtin::ConcatBytes => 2,
        Builtin::NumberToBytes => 1,
        Builtin::ShiftRight => 2,
        Builtin::BinaryAnd => 2,
        Builtin::BinaryOr => 2,
        Builtin::BytesLength => 1,
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

impl<'a> ArgType<'a> for () {
    fn from_value<'b>(value: &'b Value<'a>) -> Result<&'b Self, RuntimeError> {
        match value {
            Value::Unit => Ok(&()),
            _ => Err(RuntimeError::TypeError),
        }
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

impl<'a> ArgType<'a> for i64 {
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

impl<'a> ArgType<'a> for Vec<u8> {
    fn from_value<'b>(value: &'b Value<'a>) -> Result<&'b Self, RuntimeError> {
        match &*value {
            Value::Bytes(bytes) => Ok(bytes),
            _ => Err(RuntimeError::TypeError),
        }
    }
}

trait ResultType<'a> {
    fn to_result(self) -> EvalResult<'a>;
}

impl<'a> ResultType<'a> for Rc<Value<'a>> {
    fn to_result(self) -> EvalResult<'a> {
        Ok(self)
    }
}

impl<'a, T: ResultType<'a>> ResultType<'a> for Result<T, RuntimeError> {
    fn to_result(self) -> EvalResult<'a> {
        self?.to_result()
    }
}

impl<'a> ResultType<'a> for Value<'a> {
    fn to_result(self) -> EvalResult<'a> {
        Ok(Rc::new(self))
    }
}

impl<'a> ResultType<'a> for i64 {
    fn to_result(self) -> EvalResult<'a> {
        Ok(Rc::new(Value::Number(self)))
    }
}

impl<'a> ResultType<'a> for String {
    fn to_result(self) -> EvalResult<'a> {
        Ok(Rc::new(Value::String(self)))
    }
}

impl<'a> ResultType<'a> for bool {
    fn to_result(self) -> EvalResult<'a> {
        Ok(Rc::new(Value::Boolean(self)))
    }
}

impl<'a> ResultType<'a> for Vec<u8> {
    fn to_result(self) -> EvalResult<'a> {
        Ok(Rc::new(Value::Bytes(self)))
    }
}

fn invoke_unary<'a, A: 'a, R, F>(f: F, args: VecDeque<Rc<Value<'a>>>) -> EvalResult<'a>
where
    A: ArgType<'a>,
    R: ResultType<'a>,
    F: Fn(&A) -> R,
{
    if args.len() != 1 {
        panic!("Invalid number of arguments");
    }
    let arg1 = args.into_iter().next().unwrap();
    f(A::from_value(arg1.as_ref())?).to_result()
}

fn invoke_binary<'a, A: 'a, B: 'a, R, F>(f: F, args: VecDeque<Rc<Value<'a>>>) -> EvalResult<'a>
where
    A: ArgType<'a>,
    B: ArgType<'a>,
    R: ResultType<'a>,
    F: Fn(&A, &B) -> R,
{
    if args.len() != 2 {
        panic!("Invalid number of arguments");
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
) -> EvalResult<'a>
where
    A: ArgType<'a>,
    B: ArgType<'a>,
    C: ArgType<'a>,
    R: ResultType<'a>,
    F: Fn(&A, &B, &C) -> R,
{
    if args.len() != 3 {
        panic!("Invalid number of arguments");
    }
    let mut iter = args.into_iter();
    f(
        A::from_value(iter.next().unwrap().as_ref())?,
        B::from_value(iter.next().unwrap().as_ref())?,
        C::from_value(iter.next().unwrap().as_ref())?,
    )
    .to_result()
}

fn print<'a>(s: &String) -> Value<'a> {
    println!("{}", s);
    Value::Unit
}

fn len(vec: &Vec<Rc<Value>>) -> i64 {
    vec.len() as i64
}

fn get<'a>(vec: &Vec<Rc<Value<'a>>>, i: &i64) -> Rc<Value<'a>> {
    if *i < 0 || *i >= vec.len() as i64 {
        panic!("vector index out of range");
    }
    vec[*i as usize].clone()
}

fn concat<'a>(a: &Vec<Rc<Value<'a>>>, b: &Vec<Rc<Value<'a>>>) -> Rc<Value<'a>> {
    let mut values = a.clone();
    values.extend(b.iter().map(|rc| rc.clone()));
    Rc::new(Value::Vec {
        values,
        vec_type: VecType::Array,
    })
}

fn add(x: &i64, y: &i64) -> i64 {
    *x + *y
}

fn sub(x: &i64, y: &i64) -> i64 {
    *x - *y
}

fn mul(x: &i64, y: &i64) -> i64 {
    *x * *y
}

fn div(x: &i64, y: &i64) -> i64 {
    *x / *y
}

fn mod_function(x: &i64, n: &i64) -> i64 {
    *x % *n
}

fn substring<'a>(s: &String, start: &i64, end: &i64) -> String {
    s[(*start as usize)..(*end as usize)].to_string()
}

fn concat_string<'a>(s1: &String, s2: &String) -> String {
    s1.to_string() + s2
}

fn get_line<'a>(&(): &()) -> String {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).unwrap();
    buffer
}

fn string_length<'a>(s: &String) -> i64 {
    s.len() as i64
}

fn number_to_string<'a>(x: &i64) -> String {
    x.to_string()
}

fn leq<'a>(x: &i64, y: &i64) -> bool {
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

fn concat_bytes(a: &Vec<u8>, b: &Vec<u8>) -> Vec<u8> {
    let mut values = a.clone();
    values.extend(b.iter().map(|rc| *rc));
    values
}

fn number_to_bytes(x: &i64) -> Vec<u8> {
    if *x >= 0 && *x <= 255 {
        let mut result = Vec::new();
        result.push(*x as u8);
        return result;
    }
    panic!("Invalid byte value");
}

fn shift_right(x: &i64, n: &i64) -> i64 {
    *x >> *n
}

fn binary_and(x: &i64, y: &i64) -> i64 {
    *x & *y
}

fn binary_or(x: &i64, y: &i64) -> i64 {
    *x | *y
}

fn bytes_length<'a>(bytes: &Vec<u8>) -> i64 {
    bytes.len() as i64
}
