#[derive(Debug)]
pub enum Token<S> {
    Keyword(Keyword),
    NumericLiteral(f64),
    StringLiteral(S),
    BooleanLiteral(bool),
    Identifier(S),
    IdentifierBinding(Option<S>),
    Comment(S),
}

#[derive(Debug)]
pub enum Keyword {
    Let,
    Match,
    Data,
    Case,
    If,
    Function,
    Tuple,
    Array,
}

#[derive(Debug)]
pub enum InvalidTokenError {
    InvalidNumericLiteral,
    InvalidVariableName,
}

pub fn read_token<'a, S, F>(s: &'a str, from_str: F) -> Result<Token<S>, InvalidTokenError>
where
    F: Fn(&'a str) -> S,
{
    let first: char = s.chars().nth(0).unwrap();
    if first.is_numeric() || first == '-' {
        let result = s.parse::<f64>();
        match result {
            Ok(x) => Ok(Token::NumericLiteral(x)),
            Err(_) => Err(InvalidTokenError::InvalidNumericLiteral),
        }
    } else {
        match first {
            '!' => Ok(Token::Comment(from_str(&s[1..]))),
            '"' => Ok(Token::StringLiteral(from_str(&s[1..]))),
            '$' => {
                if s.len() == 1 {
                    return Err(InvalidTokenError::InvalidVariableName);
                }
                match read_token(&s[1..], from_str) {
                    Ok(Token::Identifier(s)) => Ok(Token::IdentifierBinding(Some(s))),
                    _ => Err(InvalidTokenError::InvalidVariableName),
                }
            }
            _ => match s {
                "let" => Ok(Token::Keyword(Keyword::Let)),
                "match" => Ok(Token::Keyword(Keyword::Match)),
                "case" => Ok(Token::Keyword(Keyword::Case)),
                "if" => Ok(Token::Keyword(Keyword::If)),
                "data" => Ok(Token::Keyword(Keyword::Data)),
                "fn" => Ok(Token::Keyword(Keyword::Function)),
                "#" => Ok(Token::Keyword(Keyword::Tuple)),
                "@" => Ok(Token::Keyword(Keyword::Array)),
                "_" => Ok(Token::IdentifierBinding(None)),
                "true" => Ok(Token::BooleanLiteral(true)),
                "false" => Ok(Token::BooleanLiteral(false)),
                _ => Ok(Token::Identifier(from_str(s))),
            },
        }
    }
}
