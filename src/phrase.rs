use crate::located::Located;

fn is_atom_char(b: u8) -> bool {
    if !b.is_ascii() {
        // Non-ASCII bytes are allowed, and start with a 1 bit
        return true;
    }
    let c: char = b as char;
    !(c.is_whitespace() || "();,".chars().any(|c1| c == c1))
}

fn is_end(b: u8) -> bool {
    b == '\n' as u8 || b == ')' as u8
}

#[derive(Debug, PartialEq)]
pub struct Phrase<T> {
    pub head: Located<T>,
    pub rest: Vec<Located<Phrase<T>>>,
}

#[derive(Debug)]
struct Line<'a> {
    indentation: usize,
    comma: bool,
    phrase: Located<Phrase<(&'a str, bool)>>,
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
        if !b.is_ascii_whitespace() || b == '\n' as u8 {
            return &s[i..];
        }
    }
    ""
}

fn read_indentation(s: &str) -> (usize, &str) {
    let mut start = 0;
    for (i, b) in s.bytes().enumerate() {
        if b == '\n' as u8 {
            // Skip empty lines
            start = i + 1;
            continue;
        }
        if b != ' ' as u8 {
            return (i - start, &s[i..]);
        }
    }
    (s.len() - start, "")
}

fn phrase(s: &str, length: usize) -> Result<(Located<Phrase<(&str, bool)>>, &str), usize> {
    if s.as_bytes()[0] == '(' as u8 {
        let (result, s1) = inner_phrase(skip_whitespace(&s[1..]), length)?;
        if s1.is_empty() || s1.as_bytes()[0] != ')' as u8 {
            return Err(length - s1.len());
        }
        return Ok(locate(result.value, s, &s1[1..], length));
    } else if s.as_bytes()[0] == ';' as u8 {
        let (result, s1) = inner_phrase(skip_whitespace(&s[1..]), length)?;
        return Ok(locate(result.value, s, s1, length));
    }
    let (head, s1) = read_atom(s, length)?;
    Ok((
        Located::new(
            head.source_range.clone(),
            Phrase {
                head,
                rest: Vec::new(),
            },
        ),
        s1,
    ))
}

fn inner_phrase(s: &str, length: usize) -> Result<(Located<Phrase<(&str, bool)>>, &str), usize> {
    let (head, mut current_s) = read_atom(s, length)?;
    let mut rest = Vec::new();
    while !current_s.is_empty() && !is_end(current_s.as_bytes()[0]) {
        let (p, s1) = phrase(current_s, length)?;
        current_s = s1;
        rest.push(p);
    }
    let end = match rest.last() {
        None => head.source_range.end,
        Some(l) => l.source_range.end,
    };
    Ok((
        Located::new(head.source_range.start..end, Phrase { head, rest }),
        current_s,
    ))
}

fn line_phrase(s: &str, length: usize) -> Result<Option<(Line, &str)>, usize> {
    let (indentation, s) = read_indentation(s);
    if s.is_empty() {
        return Ok(None);
    }
    let (s, comma) = if s.as_bytes()[0] == ',' as u8 {
        (skip_whitespace(&s[1..]), true)
    } else {
        (s, false)
    };
    let (phrase, s) = inner_phrase(s, length)?;
    Ok(Some((
        Line {
            indentation,
            comma,
            phrase,
        },
        s,
    )))
}

fn merge_lines(lines: &mut Vec<Line>, min_indentation: usize) {
    let mut siblings: Option<(usize, Vec<Line>)> = None;
    loop {
        match lines.pop() {
            None => break,
            Some(mut line) => {
                siblings = match siblings {
                    None => Some((line.indentation, vec![line])),
                    Some((indentation, mut sibling_lines)) => {
                        if line.indentation < indentation
                            || line.indentation == indentation && !line.comma
                        {
                            for child in sibling_lines.into_iter().rev() {
                                line.phrase.source_range.end = child.phrase.source_range.end;
                                line.phrase.value.rest.push(child.phrase);
                            }
                            Some((line.indentation, vec![line]))
                        } else {
                            assert_eq!(line.indentation, indentation);
                            assert!(line.comma);
                            sibling_lines.push(line);
                            Some((indentation, sibling_lines))
                        }
                    }
                };
                match siblings {
                    None => unreachable!(),
                    Some((indentation, _)) => {
                        if indentation < min_indentation {
                            break;
                        }
                    }
                }
            }
        }
    }
    for line in siblings
        .map(|t| t.1)
        .unwrap_or(Vec::new())
        .into_iter()
        .rev()
    {
        lines.push(line);
    }
}

impl Phrase<&str> {
    pub fn parse(mut s: &str) -> Result<Located<Phrase<(&str, bool)>>, usize> {
        let length = s.len();
        let mut lines: Vec<Line> = Vec::new();
        loop {
            let (line, s1) = match line_phrase(s, length)? {
                None => break,
                Some(x) => x,
            };
            s = s1;
            merge_lines(&mut lines, line.indentation + 1);
            lines.push(line);
        }
        merge_lines(&mut lines, 0);
        if lines.len() > 1 {
            return Err(lines[1].phrase.source_range.start);
        }
        if lines.len() == 0 {
            return Err(s.len());
        }
        Ok(lines.into_iter().next().unwrap().phrase)
    }
}

impl<T> Phrase<T> {
    pub fn as_atom(&self) -> Option<&Located<T>> {
        if !self.rest.is_empty() {
            return None;
        }
        Some(&self.head)
    }

    pub fn map<U, F, E>(self, f: &F) -> Result<Phrase<U>, E>
    where
        F: Fn(Located<T>) -> Result<U, E>,
    {
        let head = Located::new(self.head.source_range.clone(), f(self.head)?);
        let mut rest = Vec::new();
        for x in self.rest.into_iter() {
            rest.push(x.map(|x| x.map(f))?);
        }
        Ok(Phrase { head, rest })
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

    fn phrase(
        range: Range<usize>,
        s: Located<&str>,
        rest: Vec<Located<Phrase<String>>>,
    ) -> Located<Phrase<String>> {
        Located::new(
            range,
            Phrase {
                head: Located::new(s.source_range, s.value.to_string()),
                rest,
            },
        )
    }

    fn simple_atom(start: usize, s: &str) -> Located<Phrase<String>> {
        let range = start..start + s.len();
        phrase(range.clone(), Located::new(range, s), Vec::new())
    }

    fn parse_string(s: &str, quoted: bool) -> Result<String, usize> {
        match quoted {
            true => unescape_string(s),
            false => Ok(s.to_string()),
        }
    }

    fn parse_simple(s: &str) -> Result<Located<Phrase<String>>, usize> {
        Phrase::parse(s)?.map(|p| {
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
            parse_simple("x y z").unwrap(),
            phrase(
                0..5,
                Located::new(0..1, "x"),
                vec![simple_atom(2, "y"), simple_atom(4, "z")]
            )
        );
    }

    #[test]
    fn trailing_whitespace() {
        assert_eq!(parse_simple("x \n ").unwrap(), simple_atom(0, "x"));
    }

    #[test]
    fn semicolon() {
        assert_eq!(
            parse_simple("a; b (c; d)").unwrap(),
            phrase(
                0..11,
                Located::new(0..1, "a"),
                vec![phrase(
                    1..11,
                    Located::new(3..4, "b"),
                    vec![phrase(
                        5..11,
                        Located::new(6..7, "c"),
                        vec![phrase(7..10, Located::new(9..10, "d"), vec![])]
                    )]
                )]
            )
        )
    }

    #[test]
    fn quotation() {
        assert_eq!(
            parse_simple("[abc \\\\ \\] def]").unwrap(),
            phrase(0..15, Located::new(0..15, "abc \\ ] def"), vec![])
        );
    }

    #[test]
    fn nested_children() {
        assert_eq!(
            parse_simple("a\nb\nc").unwrap(),
            phrase(
                0..5,
                Located::new(0..1, "a"),
                vec![phrase(
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
            phrase(
                0..16,
                Located::new(0..1, "a"),
                vec![
                    phrase(4..9, Located::new(4..5, "b"), vec![simple_atom(8, "c")]),
                    simple_atom(13, "d"),
                    simple_atom(15, "e")
                ]
            )
        );
    }
}
