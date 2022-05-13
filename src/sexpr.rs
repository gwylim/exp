use nom::branch::alt;
use nom::bytes::complete::{take_while, take_while1};
use nom::character::complete::{char as nom_char, multispace0, none_of, one_of};
use nom::combinator::{all_consuming, opt};
use nom::error::ParseError;
use nom::multi::many0;
use nom::sequence::{delimited, preceded, terminated};
use nom::IResult;

#[derive(Debug, PartialEq)]
pub struct Sexpr {
    pub comment: Option<String>,
    pub operator: Option<char>,
    pub contents: SexprBody,
}

#[derive(Debug, PartialEq)]
pub enum SexprBody {
    Atom(String),
    List(Vec<Sexpr>),
}

fn whitespace_terminated<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    terminated(inner, multispace0)
}

fn escaped_char(i: &str) -> IResult<&str, char> {
    preceded(nom_char('\\'), alt((nom_char('\\'), nom_char(']'))))(i)
}

fn quoted(i: &str) -> IResult<&str, String> {
    let (rest, result) = delimited(
        nom_char('['),
        many0(alt((escaped_char, none_of("]")))),
        whitespace_terminated(nom_char(']')),
    )(i)?;
    let string: String = result.into_iter().collect();
    Ok((rest, string))
}

fn unquoted(i: &str) -> IResult<&str, String> {
    let (rest, s) = whitespace_terminated(take_while1(|c: char| {
        c.is_alphanumeric() || c == '_' || c == '-'
    }))(i)?;
    Ok((rest, s.to_string()))
}

fn atom(i: &str) -> IResult<&str, SexprBody> {
    let (rest, s) = alt((quoted, unquoted))(i)?;
    Ok((rest, SexprBody::Atom(s)))
}

fn delimited_sexpr(i: &str) -> IResult<&str, SexprBody> {
    alt((
        delimited(
            whitespace_terminated(nom_char('(')),
            sexpr,
            whitespace_terminated(nom_char(')')),
        ),
        atom,
    ))(i)
}

// TODO: allow nested comments and escaping }
fn comment(i: &str) -> IResult<&str, String> {
    let (rest, s) = delimited(
        nom_char('{'),
        take_while(|c| c != '}'),
        whitespace_terminated(nom_char('}')),
    )(i)?;
    Ok((rest, s.to_string()))
}

fn operator(i: &str) -> IResult<&str, char> {
    whitespace_terminated(one_of("$#\""))(i)
}

fn prefixed_sexpr<F>(inner: F) -> impl FnMut(&str) -> IResult<&str, Sexpr>
where
    F: Fn(&str) -> IResult<&str, SexprBody>,
{
    move |i: &str| {
        let (rest, comment) = opt(comment)(i)?;
        let (rest, operator) = opt(operator)(rest)?;
        let (rest, contents) = inner(rest)?;
        Ok((
            rest,
            Sexpr {
                comment,
                operator,
                contents,
            },
        ))
    }
}

fn sexpr(i: &str) -> IResult<&str, SexprBody> {
    let (rest, mut sexprs) = many0(prefixed_sexpr(delimited_sexpr))(i)?;
    let (rest, tail) = opt(prefixed_sexpr(list_tail))(rest)?;
    if let Some(last) = tail {
        sexprs.push(last);
    }
    Ok((rest, SexprBody::List(sexprs)))
}

fn list_tail(i: &str) -> IResult<&str, SexprBody> {
    preceded(whitespace_terminated(nom_char(';')), sexpr)(i)
}

pub fn sexpr_file(i: &str) -> IResult<&str, Sexpr> {
    let (rest, body) = all_consuming(preceded(multispace0, sexpr))(i)?;
    Ok((
        rest,
        Sexpr {
            comment: None,
            operator: None,
            contents: body,
        },
    ))
}

#[cfg(test)]
mod test {
    use super::*;

    fn new_atom(s: &str) -> Sexpr {
        Sexpr {
            comment: None,
            operator: None,
            contents: SexprBody::Atom(s.to_string()),
        }
    }

    fn new_list(exprs: Vec<Sexpr>) -> Sexpr {
        Sexpr {
            comment: None,
            operator: None,
            contents: SexprBody::List(exprs),
        }
    }

    #[test]
    fn atom() {
        assert_eq!(
            sexpr_file("x").unwrap(),
            ("", new_list(vec![new_atom("x")]))
        );
    }

    #[test]
    fn list() {
        assert_eq!(
            sexpr_file("(x y z)").unwrap(),
            (
                "",
                new_list(vec![new_list(
                    vec!["x", "y", "z"].into_iter().map(new_atom).collect()
                )])
            )
        );
    }

    #[test]
    fn leading_whitespace() {
        assert_eq!(
            sexpr_file(" \n x").unwrap(),
            ("", new_list(vec![new_atom("x")]))
        )
    }

    #[test]
    fn trailing_whitespace() {
        assert_eq!(
            sexpr_file("x \n ").unwrap(),
            ("", new_list(vec![new_atom("x")]))
        )
    }

    #[test]
    fn operators() {
        assert_eq!(
            sexpr_file("let $x (fn $y #(y y)) x").unwrap(),
            (
                "",
                new_list(vec![
                    new_atom("let"),
                    Sexpr {
                        comment: None,
                        operator: Some('$'),
                        contents: SexprBody::Atom("x".to_string())
                    },
                    new_list(vec![
                        new_atom("fn"),
                        Sexpr {
                            comment: None,
                            operator: Some('$'),
                            contents: SexprBody::Atom("y".to_string())
                        },
                        Sexpr {
                            comment: None,
                            operator: Some('#'),
                            contents: SexprBody::List(vec![new_atom("y"), new_atom("y")])
                        }
                    ]),
                    new_atom("x")
                ])
            )
        )
    }

    #[test]
    fn comments() {
        assert_eq!(
            sexpr_file("{a comment} a ({another comment} b c)").unwrap(),
            (
                "",
                new_list(vec![
                    Sexpr {
                        comment: Some("a comment".to_string()),
                        operator: None,
                        contents: SexprBody::Atom("a".to_string()),
                    },
                    new_list(vec![
                        Sexpr {
                            comment: Some("another comment".to_string()),
                            operator: None,
                            contents: SexprBody::Atom("b".to_string())
                        },
                        new_atom("c")
                    ])
                ])
            )
        )
    }

    #[test]
    fn semicolon() {
        assert_eq!(
            sexpr_file("a; b; (c; d)").unwrap(),
            (
                "",
                new_list(vec![
                    new_atom("a"),
                    new_list(vec![
                        new_atom("b"),
                        new_list(vec![new_list(vec![
                            new_atom("c"),
                            new_list(vec![new_atom("d")])
                        ])])
                    ])
                ])
            )
        )
    }

    #[test]
    fn quotation() {
        assert_eq!(
            sexpr_file("[abc \\\\ \\] def]").unwrap(),
            ("", new_list(vec![new_atom("abc \\ ] def")]))
        )
    }
}
