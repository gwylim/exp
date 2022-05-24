mod builtin;
mod eq;
mod expr;
mod format;
mod interpreter;
mod located;
mod sexpr;
mod token;
mod value;

use crate::expr::{compile, ParseError};
use crate::format::format;
use crate::interpreter::run;
use crate::sexpr::parse;
use crate::token::InvalidTokenError;
use clap::{Parser, Subcommand};
use std::fs;
use std::ops::Range;
use std::path::PathBuf;

#[derive(Parser)]
#[clap(version = "0.1.0")]
#[clap(propagate_version = true)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Run { file: PathBuf },
    Format { file: PathBuf },
}

fn get_line_end(s: &str, mut i: usize) -> usize {
    while i < s.len() && s.as_bytes()[i] != ('\n' as u8) {
        i += 1;
    }
    i
}

fn print_file_error(
    file_name: &PathBuf,
    file_contents: &str,
    source_range: Range<usize>,
    message: &str,
) {
    let mut line_count = 0;
    let mut line_start = 0;
    let line_end = loop {
        let line_end = get_line_end(file_contents, line_start);
        if line_end > source_range.start || line_end + 1 >= file_contents.len() {
            break line_end;
        }
        line_count += 1;
        line_start = line_end + 1;
    };
    eprintln!(
        "{}:{}:{}: {}",
        file_name
            .file_name()
            .unwrap()
            .to_str()
            .unwrap_or("<non-UTF-8 filename>"),
        line_count + 1,
        source_range.start - line_start,
        message
    );
    let line_number = format!("{} ", line_count + 1);
    let padding: String = (0..line_number.len()).map(|_| ' ').collect();
    eprintln!("{} |", padding);
    eprintln!("{} | {}", line_number, &file_contents[line_start..line_end]);
    let range_indicator: String = (0..(source_range.end - line_start))
        .map(|i| {
            if i < source_range.start - line_start {
                ' '
            } else {
                '^'
            }
        })
        .collect();
    eprintln!("{} | {}", padding, range_indicator);
}

fn main() {
    let args: Args = Args::parse();
    match args.command {
        Command::Run { file } => {
            let input = fs::read_to_string(file.clone()).expect("Failed to read file");
            match compile(&input) {
                Ok(expr) => {
                    println!("{}", run(&expr).expect("Exception in execution"));
                }
                Err(err) => {
                    print_file_error(
                        &file,
                        &input,
                        err.source_range,
                        match err.value {
                            ParseError::InvalidToken(_) => "Invalid token",
                            ParseError::UnexpectedCharacter => "Unexpected character",
                            ParseError::InvalidVariableBinding => "Invalid variable binding",
                            ParseError::CyclicDefinition => "Cycle in definition",
                            ParseError::UndefinedVariable => "Not in scope",
                            ParseError::InvalidExpressionInPattern => {
                                "Invalid expression in pattern"
                            }
                            ParseError::InvalidSyntax => "Invalid syntax",
                            ParseError::InvalidFieldDeclaration => {
                                "Invalid field declaration; named fields are not yet supported"
                            }
                            ParseError::UnexpectedToken => "Unexpected token",
                        },
                    );
                }
            }
        }
        Command::Format { file } => {
            let input = fs::read_to_string(file.clone()).expect("Failed to read file");
            let mut output = String::new();
            format(&input, &mut output).expect("Invalid file");
            fs::write(file, output).expect("Failed to write file");
        }
    }
}
