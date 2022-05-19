use std::convert::TryFrom;

#[derive(Debug)]
pub enum Token {
    Keyword(Keyword),
    NumericLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    Identifier(String),
    IdentifierBinding(Option<String>),
    Comment,
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

impl TryFrom<String> for Token {
    type Error = InvalidTokenError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        let first: char = s.chars().nth(0).unwrap();
        if first.is_numeric() || first == '-' {
            let result = s.parse::<f64>();
            match result {
                Ok(x) => Ok(Token::NumericLiteral(x)),
                Err(_) => Err(InvalidTokenError::InvalidNumericLiteral),
            }
        } else {
            match first {
                '!' => Ok(Token::Comment),
                '"' => Ok(Token::StringLiteral(s[1..].to_string())),
                '$' => {
                    if s.len() == 1 {
                        return Err(InvalidTokenError::InvalidVariableName);
                    }
                    match Token::try_from(s[1..].to_string()) {
                        Ok(Token::Identifier(s)) => Ok(Token::IdentifierBinding(Some(s))),
                        _ => Err(InvalidTokenError::InvalidVariableName),
                    }
                }
                _ => match s.as_str() {
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
                    _ => Ok(Token::Identifier(s)),
                },
            }
        }
    }
}
