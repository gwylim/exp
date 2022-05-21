use crate::located::Located;
use std::iter::once;
use std::ops::Range;

fn is_atom_char(b: u8) -> bool {
    if (b & 127) == 1 {
        // Non-ASCII bytes are allowed, and start with a 1 bit
        return true;
    }
    let c: char = b as char;
    !(c.is_whitespace() || "()[];".chars().any(|c1| c == c1))
}

struct StackEntry<T> {
    start: usize,
    list: Vec<Located<Sexpr<T>>>,
    // True if the list is created by a semicolon. In this case, when we get to a closing
    // parenthesis, the parent list ends as well.
    end_parent: bool,
}

#[derive(Debug, PartialEq)]
pub enum Sexpr<T> {
    Atom(T),
    List(Vec<Located<Sexpr<T>>>),
}

fn handle_escaped_string<'a, F>(s: &'a str, mut handle_part: F) -> Result<(), usize>
where
    F: FnMut(&'a str),
{
    let mut start = 0;
    let mut escaped = false;
    for (i, b) in s.bytes().enumerate() {
        match b {
            b']' => {
                if escaped {
                    handle_part("]");
                    escaped = false;
                } else {
                    panic!("Unexpected end of string character");
                }
            }
            b'\\' => {
                if escaped {
                    handle_part("\\");
                    escaped = false;
                } else {
                    handle_part(&s[start..i]);
                    start = i + 2;
                    escaped = true;
                }
            }
            _ => {
                if escaped {
                    handle_part(match b {
                        b'n' => Ok::<&str, usize>("\n"),
                        b'r' => Ok("\r"),
                        b't' => Ok("\t"),
                        b'0' => Ok("\0"),
                        _ => return Err(i - 1),
                    }?);
                    escaped = false;
                }
            }
        }
    }
    if escaped {
        return Err(s.len() - 1);
    }
    handle_part(&s[start..s.len()]);
    Ok(())
}

fn validate_escaped_string(s: &str) -> Result<(), usize> {
    handle_escaped_string(s, |_| {})
}

pub fn unescape_string(s: &str) -> Result<String, usize> {
    let mut parts: Vec<&str> = Vec::new();
    handle_escaped_string(s, |part| parts.push(part))?;
    Ok(parts.concat())
}

fn char_range(i: usize) -> Range<usize> {
    i..(i + 1)
}

pub fn parse<T, F, E>(
    s: &str,
    get_token: F,
    unexpected_character: E,
) -> Result<Located<Sexpr<T>>, Located<E>>
where
    F: Fn(&str, bool) -> Result<T, E>,
{
    let mut stack: Vec<StackEntry<T>> = vec![StackEntry {
        start: 0,
        list: Vec::new(),
        end_parent: false,
    }];
    let mut current_atom: Option<(usize, bool)> = None;
    for (i, b) in s.bytes().enumerate().chain(once((s.len(), ')' as u8))) {
        if is_atom_char(b) {
            match current_atom {
                None => current_atom = Some((i, false)),
                Some(_) => {}
            }
            continue;
        } else {
            // Non-atom chars are always ASCII
            let c: char = b as char;
            let ended_quote = match current_atom {
                None => false,
                Some((start, quoted)) => {
                    if quoted {
                        if c != ']' {
                            continue;
                        }
                        let mut j = i;
                        let bytes = s.as_bytes();
                        while j > start + 1 && bytes[j - 1] == ('\\' as u8) {
                            j -= 1;
                        }
                        let escape_characters = i - j;
                        if escape_characters % 2 == 1 {
                            continue;
                        }
                        let escaped_string = &s[(start + 1)..i];
                        match validate_escaped_string(escaped_string) {
                            Ok(_) => {}
                            Err(j) => {
                                return Err(Located::new(
                                    (start + 1 + j)..(start + 1 + j + 1),
                                    unexpected_character,
                                ))
                            }
                        }
                        current_atom = None;
                        match get_token(escaped_string, true) {
                            Ok(token) => stack
                                .last_mut()
                                .unwrap()
                                .list
                                .push(Located::new(start..(i + 1), Sexpr::Atom(token))),
                            Err(e) => return Err(Located::new(start..(i + 1), e)),
                        }
                        true
                    } else {
                        let atom = &s[start..i];
                        current_atom = None;
                        match get_token(atom, false) {
                            Ok(token) => stack
                                .last_mut()
                                .unwrap()
                                .list
                                .push(Located::new(start..i, Sexpr::Atom(token))),
                            Err(e) => return Err(Located::new(start..i, e)),
                        }
                        false
                    }
                }
            };
            if c.is_whitespace() {
                continue;
            }
            match c {
                '[' => current_atom = Some((i, true)),
                ']' => {
                    if !ended_quote {
                        return Err(Located::new(char_range(i), unexpected_character));
                    }
                }
                '(' => {
                    stack.push(StackEntry {
                        start: i,
                        list: Vec::new(),
                        end_parent: false,
                    });
                }
                ')' => loop {
                    let StackEntry {
                        list,
                        start,
                        end_parent,
                    } = match stack.pop() {
                        None => return Err(Located::new(char_range(i), unexpected_character)),
                        Some(l) => l,
                    };
                    match stack.last_mut() {
                        None => {
                            if i != s.len() {
                                return Err(Located::new(char_range(i), unexpected_character));
                            }
                            // This is the end of the program, return
                            return match list.first() {
                                None => Ok(Located::new(0..0, Sexpr::List(list))),
                                Some(first) => Ok(Located::new(
                                    first.source_range.start..list.last().unwrap().source_range.end,
                                    Sexpr::List(list),
                                )),
                            };
                        }
                        Some(StackEntry {
                            start: _,
                            list: parent_list,
                            end_parent: _,
                        }) => parent_list.push(Located::new(
                            start..(if end_parent { i } else { i + 1 }),
                            Sexpr::List(list),
                        )),
                    }
                    if !end_parent {
                        break;
                    }
                },
                ';' => {
                    stack.push(StackEntry {
                        start: i,
                        list: Vec::new(),
                        end_parent: true,
                    });
                }
                _ => unreachable!(),
            }
        }
    }
    Err(Located::new(s.len() - 1..s.len(), unexpected_character))
}

#[cfg(test)]
mod test {
    use super::*;
    use std::ops::Range;

    fn new_atom(s: &str, range: Range<usize>) -> Located<Sexpr<String>> {
        Located::new(range, Sexpr::Atom(s.to_string()))
    }

    fn new_list(exprs: Vec<Located<Sexpr<String>>>, range: Range<usize>) -> Located<Sexpr<String>> {
        Located::new(range, Sexpr::List(exprs))
    }

    fn parse_string(s: &str, escaped: bool) -> Result<String, ()> {
        match escaped {
            true => unescape_string(s),
            false => Ok(s.to_string()),
        }
        .map_err(|_| ())
    }

    fn parse_simple(s: &str) -> Result<Located<Sexpr<String>>, Located<()>> {
        parse(s, parse_string, ())
    }

    #[test]
    fn atom() {
        assert_eq!(
            parse_simple("x").unwrap(),
            new_list(vec![new_atom("x", 0..1)], 0..1)
        );
    }

    #[test]
    fn list() {
        assert_eq!(
            parse_simple("(x y z)").unwrap(),
            new_list(
                vec![new_list(
                    vec![
                        new_atom("x", 1..2),
                        new_atom("y", 3..4),
                        new_atom("z", 5..6)
                    ],
                    0..7
                )],
                0..7
            )
        );
    }

    #[test]
    fn leading_whitespace() {
        assert_eq!(
            parse_simple(" \n x").unwrap(),
            new_list(vec![new_atom("x", 3..4)], 3..4)
        );
    }

    #[test]
    fn trailing_whitespace() {
        assert_eq!(
            parse_simple("x \n ").unwrap(),
            new_list(vec![new_atom("x", 0..1)], 0..1)
        );
    }

    #[test]
    fn semicolon() {
        assert_eq!(
            parse_simple("a; b; (c; d)").unwrap(),
            new_list(
                vec![
                    new_atom("a", 0..1),
                    new_list(
                        vec![
                            new_atom("b", 3..4),
                            new_list(
                                vec![new_list(
                                    vec![
                                        new_atom("c", 7..8),
                                        new_list(vec![new_atom("d", 10..11)], 8..11)
                                    ],
                                    6..12
                                )],
                                4..12
                            )
                        ],
                        1..12
                    )
                ],
                0..12
            )
        )
    }

    #[test]
    fn quotation() {
        assert_eq!(
            parse_simple("[abc \\\\ \\] def]").unwrap(),
            new_list(vec![new_atom("abc \\ ] def", 0..15)], 0..15)
        );
    }
}
