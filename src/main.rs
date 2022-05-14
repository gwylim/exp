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
use clap::{Parser, Subcommand};
use std::fs;

#[derive(Parser)]
#[clap(version = "0.1.0")]
#[clap(propagate_version = true)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Run { file: std::path::PathBuf },
}

fn main() {
    let Args {
        command: Command::Run { file },
    } = Args::parse();
    let input = fs::read_to_string(file).expect("Failed to read file");
    let sexpr: Sexpr = sexpr::sexpr_file(&input).expect("Failed to parse sexpr").1;
    let program: Expr = compile(&sexpr).expect("Syntax error");
    println!("{:#?}", run(&program).expect("Exception in execution"));
}
