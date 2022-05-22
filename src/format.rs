use crate::sexpr::Sexpr;
use crate::token::{Keyword, Token};
use crate::{parse, token, InvalidTokenError};
use std::fmt;
use std::fmt::Write;

const LINE_LENGTH_LIMIT: usize = 72;

#[derive(Debug)]
pub enum Error {
    InvalidToken(InvalidTokenError),
    UnexpectedCharacter,
    FormatError(fmt::Error),
}

fn new_line<W: Write>(indentation: usize, write: &mut W) -> fmt::Result {
    write.write_str("\n")?;
    write.write_str(&" ".repeat(indentation))
}

fn format_token<W: Write>(token: &Token<(&str, bool)>, write: &mut W) -> fmt::Result {
    match token {
        Token::Keyword(k) => write!(
            write,
            "{}",
            match k {
                Keyword::Let => "let",
                Keyword::Match => "match",
                Keyword::Data => "data",
                Keyword::Case => "case",
                Keyword::If => "if",
                Keyword::Function => "fn",
                Keyword::Tuple => "#",
                Keyword::Array => "@",
            }
        ),
        Token::NumericLiteral(x) => write!(write, "{}", x),
        Token::StringLiteral((s, quoted)) => {
            if *quoted {
                write!(write, "[\"{}]", s)
            } else {
                write!(write, "\"{}", s)
            }
        }
        Token::BooleanLiteral(b) => {
            if *b {
                write!(write, "true")
            } else {
                write!(write, "false")
            }
        }
        Token::Identifier((s, quoted)) => {
            if *quoted {
                write!(write, "[{}]", s)
            } else {
                write!(write, "{}", s)
            }
        }
        Token::IdentifierBinding(None) => write!(write, "_"),
        Token::IdentifierBinding(Some((s, quoted))) => {
            if *quoted {
                write!(write, "[${}]", s)
            } else {
                write!(write, "${}", s)
            }
        }
        Token::Comment((s, quoted)) => {
            if *quoted {
                write!(write, "[!{}]", s)
            } else {
                write!(write, "!{}", s)
            }
        }
    }
}

// Format S-expression to a string, splitting over multiple lines
fn format_sexpr_multiline<W: Write>(
    mut sexpr: &Sexpr<Token<(&str, bool)>>,
    is_root: bool,
    indentation: usize,
    write: &mut W,
) -> fmt::Result {
    let mut is_last = false;
    let mut should_close = false;
    loop {
        match sexpr {
            Sexpr::Atom(t) => {
                if is_last {
                    write.write_str(" ")?;
                }
                format_token(t, write)?;
                break;
            }
            Sexpr::List(list) => match list.split_last() {
                None => {
                    if is_last {
                        write.write_str(" ")?;
                    }
                    write.write_str("()")?;
                    break;
                }
                Some((last, init)) => {
                    let mut output = String::new();
                    let mut current_length = 0;
                    if is_last {
                        new_line(indentation, write)?;
                        output.write_str("; ")?;
                        current_length = indentation + 2;
                    } else if !is_root {
                        output.write_str("(")?;
                        new_line(indentation + 2, &mut output)?;
                        should_close = true;
                        current_length = indentation + 2 + 1;
                    }
                    let mut length_exceeded = false;
                    for (i, child) in init.iter().enumerate() {
                        if i > 0 {
                            output.write_str(" ")?;
                            current_length += 1;
                        }
                        if length_exceeded {
                            format_sexpr_multiline(
                                &child.value,
                                false,
                                indentation + 2,
                                &mut output,
                            )?;
                            continue;
                        }
                        let mut child_string = String::new();
                        format_sexpr_inline(&child.value, &mut child_string)?;
                        // We need to leave space at the end for the next child, unless it's the
                        // last child
                        let end_padding = if i == init.len() - 1 { 0 } else { 2 };
                        if current_length + child_string.len() + end_padding > LINE_LENGTH_LIMIT {
                            length_exceeded = true;
                            format_sexpr_multiline(
                                &child.value,
                                false,
                                indentation + 2,
                                &mut output,
                            )?;
                        } else {
                            output.write_str(&child_string)?;
                            current_length += child_string.len();
                        }
                    }
                    write.write_str(&output)?;
                    sexpr = &last.value;
                    is_last = true;
                }
            },
        }
    }
    if should_close {
        new_line(indentation, write)?;
        write.write_str(")")?;
    }
    Ok(())
}

// Format S-expression to a string (single line)
fn format_sexpr_inline<W: Write>(
    mut sexpr: &Sexpr<Token<(&str, bool)>>,
    write: &mut W,
) -> fmt::Result {
    let mut is_last = false;
    let mut should_close = false;
    loop {
        match sexpr {
            Sexpr::Atom(t) => {
                if is_last {
                    write.write_str(" ")?;
                }
                format_token(t, write)?;
                break;
            }
            Sexpr::List(list) => {
                if is_last {
                    write.write_str("; ")?;
                } else {
                    should_close = true;
                    write.write_str("(")?;
                }
                if let Some((last, init)) = list.split_last() {
                    if let Some((first, rest)) = init.split_first() {
                        format_sexpr_inline(&first.value, write)?;
                        for child in rest {
                            write.write_str(" ")?;
                            format_sexpr_inline(&child.value, write)?;
                        }
                    }
                    sexpr = &last.value;
                    is_last = true;
                }
            }
        }
    }
    if should_close {
        write.write_str(")")?;
    }
    Ok(())
}

fn get_token(s: &str, quoted: bool) -> Result<Token<(&str, bool)>, Error> {
    token::read_token(s, |s| (s, quoted)).map_err(|e| Error::InvalidToken(e))
}

// TODO: report location of errors
pub fn format<W: Write>(s: &str, write: &mut W) -> Result<(), Error> {
    let sexpr = parse(s, get_token, Error::UnexpectedCharacter)
        .map_err(|err| err.value)?
        .value;
    let mut result = String::new();
    format_sexpr_multiline(&sexpr, true, 0, &mut result).map_err(|e| Error::FormatError(e))?;
    write
        .write_str(result.trim())
        .map_err(|e| Error::FormatError(e))?;
    write.write_str("\n").map_err(|e| Error::FormatError(e))?;
    Ok(())
}
