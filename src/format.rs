use crate::token::{InvalidTokenError, Keyword, Token};
use crate::{token, Phrase};
use std::fmt;
use std::fmt::Write;

const LINE_LENGTH_LIMIT: usize = 72;

#[derive(Debug)]
pub enum Error {
    InvalidToken(InvalidTokenError),
    UnexpectedCharacter,
    FormatError(fmt::Error),
}

fn new_line<W: Write>(indentation: usize, comma: bool, write: &mut W) -> fmt::Result {
    write.write_str("\n")?;
    write.write_str(&" ".repeat(if comma { indentation - 2 } else { indentation }))?;
    if comma {
        write.write_str(", ")?;
    }
    Ok(())
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
                Keyword::Loop => "loop",
                Keyword::Next => "next",
                Keyword::Unit => "unit",
                Keyword::Tuple => "#",
                Keyword::Array => "@",
                Keyword::Var => "var",
                Keyword::End => "end",
                Keyword::Import => "import",
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

// Format S-expression to a string, splitting over multiple lines as necessary to keep to limit
fn format_multiline<W: Write>(
    mut phrase: &Phrase<Token<(&str, bool)>>,
    indentation: usize,
    mut comma: bool,
    write: &mut W,
) -> fmt::Result {
    loop {
        let Phrase { head, rest } = phrase;
        let mut line = String::new();
        new_line(indentation, comma, &mut line)?;
        format_token(&head.value, &mut line)?;
        if let Some((last, init)) = rest.split_last() {
            let mut length_exceeded = false;
            let mut has_child_lines = false;
            for child in init {
                if !length_exceeded {
                    match format_inline(
                        &child.value,
                        LINE_LENGTH_LIMIT.saturating_sub(line.len() + 1),
                    )? {
                        None => {
                            length_exceeded = true;
                            write.write_str(&line)?;
                        }
                        Some(s) => {
                            line.write_str(" ")?;
                            line.write_str(&s)?;
                        }
                    }
                }
                if length_exceeded {
                    format_multiline(&child.value, indentation + 2, has_child_lines, write)?;
                    has_child_lines = true;
                }
            }
            if !length_exceeded {
                write.write_str(&line)?;
                if let Some(last_string) = format_inline(
                    &last.value,
                    LINE_LENGTH_LIMIT.saturating_sub(line.len() + 1),
                )? {
                    write.write_str(" ")?;
                    write.write_str(&last_string)?;
                    return Ok(());
                }
            }
            phrase = &last.value;
            comma = false;
        } else {
            write.write_str(&line)?;
            break;
        }
    }
    Ok(())
}

// Format S-expression to a string (single line)
fn format_inline(
    mut phrase: &Phrase<Token<(&str, bool)>>,
    max_length: usize,
) -> Result<Option<String>, fmt::Error> {
    let mut output = String::new();
    let mut is_last = false;
    let mut should_close = false;
    loop {
        let Phrase { head, rest } = phrase;
        if is_last {
            if rest.is_empty() {
                output.write_str(" ")?;
            } else {
                output.write_str("; ")?;
            }
        } else if !rest.is_empty() {
            output.write_str("(")?;
            should_close = true;
        }
        format_token(&head.value, &mut output)?;
        if let Some((last, init)) = rest.split_last() {
            for child in init {
                output.write_str(" ")?;
                if output.len() >= max_length {
                    return Ok(None);
                }
                if let Some(child_string) = format_inline(&child.value, max_length - output.len())?
                {
                    output.write_str(&child_string)?;
                } else {
                    return Ok(None);
                }
            }
            phrase = &last.value;
            is_last = true;
        } else {
            break;
        }
    }
    if should_close {
        output.write_str(")")?;
    }
    Ok(if output.len() < max_length {
        Some(output)
    } else {
        None
    })
}

fn get_token(s: &str, quoted: bool) -> Result<Token<(&str, bool)>, Error> {
    token::read_token(s, |s| (s, quoted)).map_err(|e| Error::InvalidToken(e))
}

// TODO: report location of errors
pub fn format<W: Write>(s: &str, write: &mut W) -> Result<(), Error> {
    let phrase = Phrase::parse(s)
        .map_err(|_i| Error::UnexpectedCharacter)?
        .map(|p| {
            p.map(&|l| {
                let (s, quoted) = l.value;
                get_token(s, quoted)
            })
        })?;
    let mut result = String::new();
    format_multiline(&phrase.value, 0, false, &mut result).map_err(|e| Error::FormatError(e))?;
    write
        .write_str(result.trim())
        .map_err(|e| Error::FormatError(e))?;
    write.write_str("\n").map_err(|e| Error::FormatError(e))?;
    Ok(())
}
