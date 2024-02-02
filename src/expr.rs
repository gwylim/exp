use crate::located::Located;

fn is_atom_char(b: u8) -> bool {
    if !b.is_ascii() {
        // Non-ASCII bytes are allowed, and start with a 1 bit
        return true;
    }
    let c: char = b as char;
    !(c.is_whitespace() || "()".chars().any(|c1| c == c1))
}

fn is_end(b: u8) -> bool {
    b == '\n' as u8 || b == ')' as u8
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<T> {
    Atom(T),
    Cons(Box<Expr<T>>, Box<Expr<T>>),
    Nil,
}

// Returns the located version of a token using the string before and after parsing it and the total
// length of the input. Also returns the next string to use after skipping whitespace.
fn locate<'a, T>(value: T, s: &str, rest: &'a str, length: usize) -> (Located<T>, &'a str) {
    (
        Located::new(length - s.len()..length - rest.len(), value),
        skip_whitespace(rest),
    )
}

fn read_atom(s: &str, length: usize) -> Result<(Located<(&str, bool)>, &str), usize> {
    if s.is_empty() {
        return Err(length);
    }
    if s.as_bytes()[0] == '[' as u8 {
        for (i, b) in s.bytes().enumerate() {
            if b != ']' as u8 {
                continue;
            }
            let mut j = i;
            let bytes = s.as_bytes();
            while j > 1 && bytes[j - 1] == ('\\' as u8) {
                j -= 1;
            }
            let escape_characters = i - j;
            if escape_characters % 2 == 1 {
                continue;
            }
            let escaped_string = &s[1..i];
            return match validate_escaped_string(escaped_string) {
                Ok(_) => Ok(locate((escaped_string, true), s, &s[i + 1..], length)),
                Err(j) => Err(length - s.len() + 1 + j),
            };
        }
        Err(s.len())
    } else {
        for (i, b) in s.bytes().enumerate() {
            if !is_atom_char(b) {
                if i == 0 {
                    return Err(length - s.len());
                }
                return Ok(locate((&s[..i], false), s, &s[i..], length));
            }
        }
        Ok(locate((s, false), s, "", length))
    }
}

fn skip_whitespace(s: &str) -> &str {
    for (i, b) in s.bytes().enumerate() {
        if !b.is_ascii_whitespace() {
            return &s[i..];
        }
    }
    ""
}

fn expr(s: &str, length: usize) -> Result<(Located<Expr<&str>>, &str), usize> {
    if s.as_bytes()[0] == '(' as u8 {
        let (result, s1) = inner_expr(skip_whitespace(&s[1..]), length)?;
        if s1.is_empty() || s1.as_bytes()[0] != ')' as u8 {
            return Err(length - s1.len());
        }
        return Ok(locate(result.value, s, &s1[1..], length));
    }
    // TODO: do this non-recursively
    return inner_expr(skip_whitespace(s), length);
}

fn inner_expr(s: &str, length: usize) -> Result<(Located<Expr>, &str), usize> {
    let (head, mut current_s) = read_atom(s, length)?;
    let mut rest = Vec::new();
    while !current_s.is_empty() && !is_end(current_s.as_bytes()[0]) {
        let (p, s1) = expr(current_s, length)?;
        current_s = s1;
        rest.push(p);
    }
    let end = match rest.last() {
        None => head.source_range.end,
        Some(l) => l.source_range.end,
    };
    Ok((
        Located::new(head.source_range.start..end, Expr { head, rest }),
        current_s,
    ))
}

impl Expr<&str> {
    pub fn parse(s: &str) -> Result<Located<Expr<&str>>, usize> {
        let (p, mut s1) = inner_expr(skip_whitespace(s), s.len())?;
        s1 = skip_whitespace(s1);
        if s1.len() > 0 {
            return Err(s.len() - s1.len());
        }
        Ok(p)
    }
}

impl<T> Expr<T> {
    pub fn as_atom(&self) -> Option<&Located<T>> {
        match self {
            Expr::Atom(s) => Some(s),
            _ => None,
        }
    }

    pub fn map<U, F: Fn(T) -> U>(self, f: F) {
        match self {
            Expr::Atom(x) => Expr::Atom(f(x)),
            Expr::Cons(x, y) => Expr::Cons(x.map(|x| x.map(f)), y.map(|y| y.map(f))),
        }
    }
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

#[cfg(test)]
mod test {
    use super::*;
    use std::ops::Range;

    fn expr(range: Range<usize>, s: Located<&str>, rest: Vec<Located<Expr>>) -> Located<Expr> {
        Located::new(
            range,
            Expr {
                head: Located::new(s.source_range, s.value.to_string()),
                rest,
            },
        )
    }

    fn simple_atom(start: usize, s: &str) -> Located<Expr> {
        let range = start..start + s.len();
        expr(range.clone(), Located::new(range, s), Vec::new())
    }

    fn parse_string(s: &str, quoted: bool) -> Result<String, usize> {
        match quoted {
            true => unescape_string(s),
            false => Ok(s.to_string()),
        }
    }

    fn parse_simple(s: &str) -> Result<Located<Expr>, usize> {
        Expr::parse(s)?.map(|p| {
            p.map(&|l| {
                let (s, quoted) = l.value;
                Ok(parse_string(s, quoted)?)
            })
        })
    }

    #[test]
    fn atom() {
        assert_eq!(parse_simple("x").unwrap(), simple_atom(0, "x"));
    }

    #[test]
    fn list() {
        assert_eq!(
            parse_simple("x (y) (z)").unwrap(),
            expr(
                0..9,
                Located::new(0..1, "x"),
                vec![
                    expr(2..5, Located::new(3..4, "y"), vec![]),
                    expr(6..9, Located::new(7..8, "z"), vec![]),
                ]
            )
        );
    }

    #[test]
    fn trailing_whitespace() {
        assert_eq!(parse_simple("x \n ").unwrap(), simple_atom(0, "x"));
    }

    #[test]
    fn final_child() {
        assert_eq!(
            parse_simple("a b (c d)").unwrap(),
            expr(
                0..9,
                Located::new(0..1, "a"),
                vec![expr(
                    2..9,
                    Located::new(2..3, "b"),
                    vec![expr(
                        4..9,
                        Located::new(5..6, "c"),
                        vec![simple_atom(7, "d")]
                    )]
                )]
            )
        )
    }

    #[test]
    fn quotation() {
        assert_eq!(
            parse_simple("[abc \\\\ \\] def]").unwrap(),
            expr(0..15, Located::new(0..15, "abc \\ ] def"), vec![])
        );
    }

    #[test]
    fn nested_children() {
        assert_eq!(
            parse_simple("a\nb\nc").unwrap(),
            expr(
                0..5,
                Located::new(0..1, "a"),
                vec![expr(
                    2..5,
                    Located::new(2..3, "b"),
                    vec![simple_atom(4, "c")]
                )]
            )
        )
    }

    #[test]
    fn multiple_children() {
        assert_eq!(
            parse_simple("a\n  b\n  c\n,  d\ne").unwrap(),
            expr(
                0..16,
                Located::new(0..1, "a"),
                vec![
                    expr(4..9, Located::new(4..5, "b"), vec![simple_atom(8, "c")]),
                    simple_atom(13, "d"),
                    simple_atom(15, "e")
                ]
            )
        );
    }
}
