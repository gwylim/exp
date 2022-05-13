use nom::branch::alt;
use nom::bytes::complete::{take_while, take_while1};
use nom::character::complete::{char as nom_char, multispace0, none_of, one_of};
use nom::combinator::{all_consuming, opt};
use nom::error::ParseError;
use nom::multi::many0;
use nom::sequence::{delimited, preceded, terminated};
use nom::IResult;

#[derive(Debug)]
pub struct Sexpr {
    pub comment: Option<String>,
    pub operator: Option<char>,
    pub contents: SexprBody,
}

#[derive(Debug)]
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
    all_consuming(preceded(multispace0, prefixed_sexpr(sexpr)))(i)
}
