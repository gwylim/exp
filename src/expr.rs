use crate::builtin::{get_builtin, Builtin};
use crate::checks::check_recursion_guarded;
use crate::located::Located;
use crate::phrase::{unescape_string, Phrase};
use crate::token::{read_token, InvalidTokenError, Keyword, Token};
use crate::value::VecType;
use std::collections::vec_deque::VecDeque;

#[derive(Debug)]
pub enum ParseError {
    InvalidToken(InvalidTokenError),
    UnexpectedCharacter,
    UnexpectedToken,
    InvalidVariableBinding,
    CyclicDefinition,
    UndefinedVariable,
    InvalidExpressionInPattern,
    InvalidSyntax,
    // Intending to support named fields, but for now they must always be _
    InvalidFieldDeclaration,
}

fn result<Expr, E>(expr: Expr) -> Result<(Expr, Vec<usize>), E> {
    Ok((expr, Vec::new()))
}

#[derive(Debug)]
pub enum PatternExpr {
    BindVar,
    Vec {
        values: Vec<PatternExpr>,
        vec_type: VecType,
    },
    Constructed {
        constructor: usize,
        arguments: Vec<PatternExpr>,
    },
    // TODO: maybe can avoid duplicating this with Expr
    NumericConstant(i64),
    StringConstant(String),
    BooleanConstant(bool),
    Unit,
    Bound(usize),
}

#[derive(Debug)]
pub enum Declaration {
    Let(Expr),
    // A type declaration is a list of constructors, each characterized by an arity
    Type(Vec<usize>),
}

// TODO: need source position information
#[derive(Debug)]
pub enum Expr {
    NumericConstant(i64),
    StringConstant(String),
    BooleanConstant(bool),
    BytesConstant(Vec<u8>),
    Unit,
    Function {
        arity: usize,
        body: Box<Expr>,
    },
    Bound(usize),
    Apply {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Builtin(Builtin),
    Declarations {
        decls: Vec<Declaration>,
        body: Box<Expr>,
    },
    Vec {
        values: Vec<Expr>,
        vec_type: VecType,
    },
    If {
        condition: Box<Expr>,
        true_branch: Box<Expr>,
        false_branch: Box<Expr>,
    },
    Match {
        value: Box<Expr>,
        cases: Vec<(PatternExpr, Expr)>,
    },
    Loop {
        initial_vars: Vec<Expr>,
        body: Box<Expr>,
    },
    Next(Vec<Expr>),
}

fn lookup(var: &str, env: &VecDeque<Option<&str>>) -> Option<usize> {
    env.iter()
        .position(|s| if let Some(v) = s { var.eq(*v) } else { false })
}

fn compile_atom(
    atom: &Token<String>,
    env: &VecDeque<Option<&str>>,
) -> Result<(Expr, Vec<usize>), ParseError> {
    match atom {
        Token::Keyword(Keyword::Unit) => result(Expr::Unit),
        Token::StringLiteral(s) => result(Expr::StringConstant(s.to_string())),
        Token::NumericLiteral(x) => result(Expr::NumericConstant(*x)),
        Token::BooleanLiteral(b) => result(Expr::BooleanConstant(*b)),
        Token::BytesLiteral(b) => result(Expr::BytesConstant(b.clone())),
        Token::Identifier(s) => match lookup(s, env) {
            None => get_builtin(s)
                .map(|b| result(Expr::Builtin(b)))
                .unwrap_or(Err(ParseError::UndefinedVariable)),
            Some(i) => Ok((Expr::Bound(i), vec![i])),
        },
        _ => Err(ParseError::InvalidSyntax),
    }
}

fn merge(v1: &[usize], v2: &[usize]) -> Vec<usize> {
    let mut i1 = 0;
    let mut i2 = 0;
    let mut result = Vec::new();
    while i1 < v1.len() || i2 < v2.len() {
        if i1 < v1.len() {
            if i2 < v2.len() {
                if v1[i1] == v2[i2] {
                    result.push(v1[i1]);
                    i1 += 1;
                    i2 += 1;
                } else if v1[i1] < v2[i2] {
                    result.push(v1[i1]);
                    i1 += 1;
                } else {
                    result.push(v2[i2]);
                    i2 += 1;
                }
            } else {
                result.push(v1[i1]);
                i1 += 1;
            }
        } else {
            result.push(v2[i2]);
            i2 += 1;
        }
    }
    result
}

fn parent_used_vars(used_vars: Vec<usize>, count: usize) -> Vec<usize> {
    used_vars
        .into_iter()
        .filter(|i| *i + 1 > count)
        .map(|i| i - count)
        .collect()
}

fn read_function_vars(
    list: &[Located<Phrase<Token<String>>>],
) -> Result<Vec<Option<&str>>, Located<ParseError>> {
    list.iter()
        .map(|phrase| match &phrase.value.head.value {
            Token::IdentifierBinding(s) => Ok(s.as_deref()),
            _ => Err(Located::new(
                phrase.source_range.clone(),
                ParseError::InvalidVariableBinding,
            )),
        })
        .collect()
}

fn compile_pattern<'a>(
    phrase: &'a Located<Phrase<Token<String>>>,
    env: &mut VecDeque<Option<&'a str>>,
    vars: &mut Vec<Option<&'a str>>,
) -> Result<(PatternExpr, Vec<usize>), Located<ParseError>> {
    let Phrase { ref head, ref rest } = phrase.value;
    match &head.value {
        Token::IdentifierBinding(s) => {
            if let Some(p) = rest.first() {
                return Err(p.with_value(ParseError::UnexpectedToken));
            }
            vars.push(s.as_deref());
            result(PatternExpr::BindVar)
        }
        Token::Keyword(Keyword::Tuple) | Token::Keyword(Keyword::Array) => {
            let mut patterns = Vec::with_capacity(rest.len());
            let mut used_vars = vec![];
            for phrase in rest {
                let (pattern, pattern_used_vars) = compile_pattern(phrase, env, vars)?;
                patterns.push(pattern);
                used_vars = merge(&used_vars, &pattern_used_vars);
            }
            let vec_type = match &head.value {
                Token::Keyword(k) => match k {
                    Keyword::Tuple => VecType::Tuple,
                    Keyword::Array => VecType::Array,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            Ok((
                PatternExpr::Vec {
                    values: patterns,
                    vec_type,
                },
                used_vars,
            ))
        }
        Token::Identifier(s) => {
            let constructor = match lookup(s, env) {
                None => Err(Located::new(
                    head.source_range.clone(),
                    ParseError::UndefinedVariable,
                )),
                Some(i) => Ok(i),
            }?;
            let mut patterns = Vec::with_capacity(rest.len());
            let mut used_vars = vec![constructor];
            for phrase in rest {
                let (pattern, pattern_used_vars) = compile_pattern(phrase, env, vars)?;
                patterns.push(pattern);
                used_vars = merge(&used_vars, &pattern_used_vars);
            }
            if patterns.is_empty() {
                Ok((PatternExpr::Bound(constructor), used_vars))
            } else {
                Ok((
                    PatternExpr::Constructed {
                        constructor,
                        arguments: patterns,
                    },
                    used_vars,
                ))
            }
        }
        _ => {
            let (expr, used_vars) = compile_phrase(phrase, env)?;
            let pattern = match expr {
                Expr::NumericConstant(n) => Ok(PatternExpr::NumericConstant(n)),
                Expr::StringConstant(s) => Ok(PatternExpr::StringConstant(s)),
                Expr::BooleanConstant(b) => Ok(PatternExpr::BooleanConstant(b)),
                Expr::Unit => Ok(PatternExpr::Unit),
                Expr::Bound(i) => Ok(PatternExpr::Bound(i)),
                _ => Err(Located::new(
                    phrase.source_range.clone(),
                    ParseError::InvalidExpressionInPattern,
                )),
            }?;
            if let Some(p) = rest.first() {
                return Err(p.with_value(ParseError::UnexpectedToken));
            }
            Ok((pattern, used_vars))
        }
    }
}

fn compile_case<'a>(
    phrase: &'a Located<Phrase<Token<String>>>,
    env: &mut VecDeque<Option<&'a str>>,
) -> Result<(PatternExpr, Expr, Vec<usize>), Located<ParseError>> {
    let Phrase { head, rest } = &phrase.value;
    match &head.value {
        Token::Keyword(Keyword::Case) => {
            if rest.len() != 2 {
                return Err(phrase.with_value(ParseError::InvalidSyntax));
            }
            let mut defined_vars = Vec::new();
            let (pattern, pattern_used_vars) = compile_pattern(&rest[0], env, &mut defined_vars)?;
            let var_count = defined_vars.len();
            for var in defined_vars {
                env.push_front(var);
            }
            let (body, body_used_vars) = compile_phrase(&rest[1], env)?;
            for _ in 0..var_count {
                env.pop_front();
            }
            Ok((
                pattern,
                body,
                merge(
                    &pattern_used_vars,
                    &parent_used_vars(body_used_vars, var_count),
                ),
            ))
        }
        _ => Err(head.with_value(ParseError::InvalidSyntax)),
    }
}

fn compile_cases<'a>(
    phrases: &'a [Located<Phrase<Token<String>>>],
    env: &mut VecDeque<Option<&'a str>>,
) -> Result<(Vec<(PatternExpr, Expr)>, Vec<usize>), Located<ParseError>> {
    let mut used_vars = vec![];
    let mut cases = Vec::with_capacity(phrases.len());
    for p in phrases {
        let (pattern_expr, body, case_used_vars) = compile_case(p, env)?;
        cases.push((pattern_expr, body));
        used_vars = merge(&used_vars, &case_used_vars);
    }
    Ok((cases, used_vars))
}

fn compile_data_constructor<'a>(
    phrase: &'a Located<Phrase<Token<String>>>,
    env: &mut VecDeque<Option<&'a str>>,
) -> Result<usize, Located<ParseError>> {
    let Phrase { ref head, ref rest } = phrase.value;
    match &head.value {
        Token::IdentifierBinding(s) => {
            env.push_front(s.as_deref());
            for v in rest {
                if let Some(Token::IdentifierBinding(None)) = v.value.as_atom().map(|l| &l.value) {
                } else {
                    return Err(Located::new(
                        v.source_range.clone(),
                        ParseError::InvalidFieldDeclaration,
                    ));
                }
            }
            Ok(rest.len())
        }
        _ => Err(Located::new(
            phrase.source_range.clone(),
            ParseError::UnexpectedToken,
        )),
    }
}

fn apply<'a>(
    function: (Expr, Vec<usize>),
    rest: &'a [Located<Phrase<Token<String>>>],
    env: &mut VecDeque<Option<&'a str>>,
) -> Result<(Expr, Vec<usize>), Located<ParseError>> {
    let (function_expr, mut used_vars) = function;
    let mut arguments = Vec::new();
    for phrase in rest {
        let (arg_expr, arg_used_vars) = compile_phrase(phrase, env)?;
        used_vars = merge(&used_vars, &arg_used_vars);
        arguments.push(arg_expr);
    }
    if arguments.is_empty() {
        Ok((function_expr, used_vars))
    } else {
        Ok((
            Expr::Apply {
                function: Box::new(function_expr),
                arguments,
            },
            used_vars,
        ))
    }
}

fn compile_loop_var<'a>(
    phrase: &'a Located<Phrase<Token<String>>>,
    env: &mut VecDeque<Option<&'a str>>,
) -> Result<(Option<&'a str>, Expr, Vec<usize>), Located<ParseError>> {
    let Phrase { head, rest } = &phrase.value;
    match &head.value {
        Token::Keyword(Keyword::Var) => {
            if rest.len() != 2 {
                return Err(phrase.with_value(ParseError::InvalidSyntax));
            }
            match &rest[0].value.as_atom() {
                Some(p) => match &p.value {
                    Token::IdentifierBinding(s) => {
                        let (value, value_used_vars) = compile_phrase(&rest[1], env)?;
                        Ok((s.as_deref(), value, value_used_vars))
                    }
                    _ => return Err(rest[0].with_value(ParseError::UnexpectedToken)),
                },
                _ => return Err(rest[0].with_value(ParseError::InvalidSyntax)),
            }
        }
        _ => Err(head.with_value(ParseError::UnexpectedToken)),
    }
}

fn compile_phrase<'a>(
    mut phrase: &'a Located<Phrase<Token<String>>>,
    env: &mut VecDeque<Option<&'a str>>,
) -> Result<(Expr, Vec<usize>), Located<ParseError>> {
    let mut decls = Vec::new();
    let mut var_count = 0;
    // Variables used from the parent environment (i.e. from the initial value of env)
    let mut used_vars = Vec::new();
    let body = loop {
        let Phrase { head, rest } = &phrase.value;
        match &head.value {
            Token::Keyword(Keyword::Tuple) | Token::Keyword(Keyword::Array) => {
                let mut values = Vec::new();
                for phrase in rest {
                    let (expr, new_used_vars) = compile_phrase(phrase, env)?;
                    values.push(expr);
                    used_vars = merge(&used_vars, &parent_used_vars(new_used_vars, var_count));
                }
                break Expr::Vec {
                    values,
                    vec_type: match &head.value {
                        Token::Keyword(Keyword::Tuple) => VecType::Tuple,
                        Token::Keyword(Keyword::Array) => VecType::Array,
                        _ => unreachable!(),
                    },
                };
            }
            Token::Keyword(Keyword::Function) => {
                if rest.len() < 2 {
                    return Err(head.with_value(ParseError::InvalidSyntax));
                }
                let (body, args) = rest.split_last().unwrap();
                let vars = read_function_vars(args)?;
                for v in vars.iter().rev() {
                    env.push_front(*v);
                }
                var_count += vars.len();
                let (expr, expr_used_vars) = compile_phrase(body, env)?;
                // Change de Bruijn indices for parent environment
                let parent_used_vars = parent_used_vars(expr_used_vars, var_count);
                used_vars = merge(&used_vars, &parent_used_vars);
                for _ in &vars {
                    env.pop_front();
                }
                var_count -= vars.len();
                break Expr::Function {
                    arity: vars.len(),
                    body: Box::new(expr),
                };
            }
            Token::Keyword(Keyword::Loop) => {
                if rest.len() < 1 {
                    return Err(phrase.with_value(ParseError::InvalidSyntax));
                }
                let mut var_names = Vec::new();
                let mut initial_vars = Vec::new();
                for i in 0..rest.len() - 1 {
                    let (var_name, initial_value, initial_value_used_vars) =
                        compile_loop_var(&rest[i], env)?;
                    var_names.push(var_name);
                    initial_vars.push(initial_value);
                    used_vars = merge(&used_vars, &initial_value_used_vars);
                }
                for v in var_names {
                    env.push_front(v);
                }
                var_count += initial_vars.len();
                let (body, body_used_vars) = compile_phrase(&rest[rest.len() - 1], env)?;
                used_vars = merge(&used_vars, &parent_used_vars(body_used_vars, var_count));
                for _ in 0..initial_vars.len() {
                    env.pop_front();
                }
                var_count -= initial_vars.len();
                break Expr::Loop {
                    initial_vars,
                    body: Box::new(body),
                };
            }
            // FIXME: check that next only occurs in valid positions
            Token::Keyword(Keyword::Next) => {
                let mut arguments = Vec::new();
                for phrase in rest {
                    let (expr, expr_used_vars) = compile_phrase(phrase, env)?;
                    used_vars = merge(&used_vars, &parent_used_vars(expr_used_vars, var_count));
                    arguments.push(expr);
                }
                break Expr::Next(arguments);
            }
            Token::Keyword(Keyword::Let) => {
                if rest.len() != 3 {
                    return Err(Located::new(
                        phrase.source_range.clone(),
                        ParseError::InvalidSyntax,
                    ));
                }
                match &rest[0].value.head.value {
                    Token::IdentifierBinding(s) => {
                        env.push_front(s.as_deref());
                        var_count += 1;
                        let (value_expression, value_used_vars) = compile_phrase(&rest[1], env)?;
                        check_recursion_guarded(&value_expression, 0)
                            .map_err(|e| Located::new(rest[0].source_range.clone(), e))?;
                        phrase = &rest[2];
                        used_vars =
                            merge(&used_vars, &parent_used_vars(value_used_vars, var_count));
                        decls.push(Declaration::Let(value_expression));
                    }
                    _ => {
                        return Err(Located::new(
                            rest[0].source_range.clone(),
                            ParseError::InvalidVariableBinding,
                        ))
                    }
                }
            }
            Token::Keyword(Keyword::If) => {
                if rest.len() != 3 {
                    return Err(Located::new(
                        phrase.source_range.clone(),
                        ParseError::InvalidSyntax,
                    ));
                }
                let (condition, condition_used_vars) = compile_phrase(&rest[0], env)?;
                let (true_branch, true_used_vars) = compile_phrase(&rest[1], env)?;
                let (false_branch, false_used_vars) = compile_phrase(&rest[2], env)?;
                used_vars = merge(
                    &used_vars,
                    &parent_used_vars(
                        merge(
                            &condition_used_vars,
                            &merge(&true_used_vars, &false_used_vars),
                        ),
                        var_count,
                    ),
                );
                break Expr::If {
                    condition: Box::new(condition),
                    true_branch: Box::new(true_branch),
                    false_branch: Box::new(false_branch),
                };
            }
            Token::Keyword(Keyword::Match) => {
                if rest.len() < 1 {
                    return Err(Located::new(
                        phrase.source_range.clone(),
                        ParseError::InvalidSyntax,
                    ));
                }
                let (value_phrase, cases_phrases) = rest.split_first().unwrap();
                let (value, value_used_vars) = compile_phrase(value_phrase, env)?;
                let (cases, cases_used_vars) = compile_cases(cases_phrases, env)?;
                used_vars = merge(
                    &used_vars,
                    &parent_used_vars(merge(&value_used_vars, &cases_used_vars), var_count),
                );
                break Expr::Match {
                    value: Box::new(value),
                    cases,
                };
            }
            Token::Keyword(Keyword::Data) => {
                if rest.len() < 2 {
                    return Err(Located::new(
                        phrase.source_range.clone(),
                        ParseError::InvalidSyntax,
                    ));
                }
                let mut constructor_var_counts = Vec::new();
                for phrase in &rest[..rest.len() - 1] {
                    constructor_var_counts.push(compile_data_constructor(phrase, env)?);
                }
                let constructor_count = constructor_var_counts.len();
                var_count += constructor_count;
                phrase = &rest.last().unwrap();
                decls.push(Declaration::Type(constructor_var_counts));
            }
            v => {
                let (expr, expr_used_vars) = apply(
                    compile_atom(v, env).map_err(|e| Located::new(head.source_range.clone(), e))?,
                    rest,
                    env,
                )?;
                used_vars = merge(&used_vars, &parent_used_vars(expr_used_vars, var_count));
                break expr;
            }
        }
    };
    for _ in 0..var_count {
        env.pop_front();
    }
    if decls.is_empty() {
        return Ok((body, used_vars));
    }
    Ok((
        Expr::Declarations {
            decls,
            body: Box::new(body),
        },
        used_vars,
    ))
}

fn strip_comments(
    phrase: Located<Phrase<Token<String>>>,
) -> Result<Located<Phrase<Token<String>>>, Located<ParseError>> {
    let Phrase { head, mut rest } = phrase.value;
    match head.value {
        Token::Comment(_) => {
            if rest.len() != 1 {
                return Err(Located::new(phrase.source_range, ParseError::InvalidSyntax));
            }
            Ok(strip_comments(rest.remove(0))?)
        }
        _ => {
            let mut new_rest = Vec::new();
            for p in rest {
                new_rest.push(strip_comments(p)?);
            }
            Ok(Located::new(
                phrase.source_range,
                Phrase {
                    head,
                    rest: new_rest,
                },
            ))
        }
    }
}

pub fn compile(s: &str) -> Result<Expr, Located<ParseError>> {
    let phrase = Phrase::parse(s)
        .map_err(|i| Located::new(i..i + 1, ParseError::UnexpectedCharacter))?
        .map(|p| {
            p.map(&|v| {
                let (s, escaped) = v.value;
                read_token(s, |s| {
                    if escaped {
                        unescape_string(s).unwrap()
                    } else {
                        s.to_string()
                    }
                })
                .map_err(|e| Located::new(v.source_range, ParseError::InvalidToken(e)))
            })
        })?;
    let phrase_no_comments = strip_comments(phrase)?;
    let (expr, used_vars) = compile_phrase(&phrase_no_comments, &mut VecDeque::new())?;
    if !used_vars.is_empty() {
        panic!("Vars referenced that don't exist, should be impossible");
    }
    Ok(expr)
}
