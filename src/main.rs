extern crate nom;

mod builtin;
mod eq;
mod interpreter;
mod program;
mod sexpr;
mod value;

use crate::interpreter::run;
use crate::program::{compile, Expr};
use crate::sexpr::Sexpr;
use std::{env, fs};

fn main() {
    let input = fs::read_to_string(env::args().nth(1).expect("File not specified"))
        .expect("Failed to read file");
    let sexpr: Sexpr = sexpr::sexpr_file(&input).expect("Failed to parse sexpr").1;
    let program: Expr = compile(&sexpr).expect("Syntax error");
    println!("{:#?}", run(&program).expect("Exception in execution"));
}
