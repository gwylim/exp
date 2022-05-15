use crate::builtin::{get_builtin, Builtin};
use crate::located::Located;
use crate::sexpr::{unescape_string, Sexpr};
use crate::token::{Keyword, Token};
use crate::{parse, InvalidTokenError};
use std::collections::vec_deque::VecDeque;
use std::convert::TryFrom;
use std::iter::once;
use std::ops::Range;

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
    EmptyInput,
}

fn result<Expr, E>(expr: Expr) -> Result<(Box<Expr>, Vec<usize>), E> {
    Ok((Box::new(expr), Vec::new()))
}

fn result_with_vars<Expr, E>(
    expr: Expr,
    used_vars: Vec<usize>,
) -> Result<(Box<Expr>, Vec<usize>), E> {
    Ok((Box::new(expr), used_vars))
}

#[derive(Debug)]
pub enum PatternExpr {
    BindVar,
    Vec(Vec<Box<PatternExpr>>),
    Constructed {
        constructor: usize,
        arguments: Vec<Box<PatternExpr>>,
    },
    // TODO: maybe can avoid duplicating this with Expr
    NumericConstant(f64),
    StringConstant(String),
    BooleanConstant(bool),
    Unit,
    Bound(usize),
}

// TODO: need source position information
#[derive(Debug)]
pub enum Expr {
    NumericConstant(f64),
    StringConstant(String),
    BooleanConstant(bool),
    Unit,
    Function {
        arity: usize,
        body: Box<Expr>,
        // List of bound variables to copy from parent environment
        env_map: Vec<usize>,
    },
    Bound(usize),
    Apply {
        function: Box<Expr>,
        arguments: Vec<Box<Expr>>,
    },
    Builtin(Builtin),
    Let {
        value: Box<Expr>,
        body: Box<Expr>,
    },
    Vec(Vec<Box<Expr>>),
    If {
        condition: Box<Expr>,
        true_branch: Box<Expr>,
        false_branch: Box<Expr>,
    },
    Match {
        value: Box<Expr>,
        cases: Vec<(PatternExpr, Box<Expr>)>,
    },
    // A type declaration is a list of constructors, each characterized by an arity
    TypeDeclaration {
        constructors: Vec<usize>,
        body: Box<Expr>,
    },
}

fn lookup(var: &str, env: &VecDeque<&str>) -> Option<usize> {
    env.iter().position(|s| var.eq(*s))
}

fn compile_atom(atom: &Token, env: &VecDeque<&str>) -> Result<(Box<Expr>, Vec<usize>), ParseError> {
    match atom {
        Token::StringLiteral(s) => result(Expr::StringConstant(s.to_string())),
        Token::NumericLiteral(x) => result(Expr::NumericConstant(*x)),
        Token::BooleanLiteral(b) => result(Expr::BooleanConstant(*b)),
        Token::Identifier(s) => match lookup(s, env) {
            None => get_builtin(s)
                .map(|b| result(Expr::Builtin(b)))
                .unwrap_or(Err(ParseError::UndefinedVariable)),
            Some(i) => result_with_vars(Expr::Bound(i), vec![i]),
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

fn read_function_vars(list: &[Located<Sexpr<Token>>]) -> Result<Vec<&str>, Located<ParseError>> {
    list.iter()
        .map(|sexpr| match &sexpr.value {
            Sexpr::Atom(Token::IdentifierBinding(s)) => Ok(s.as_deref().unwrap_or("")),
            _ => Err(Located::new(
                sexpr.source_range.clone(),
                ParseError::InvalidVariableBinding,
            )),
        })
        .collect()
}

fn var_count(pattern: &PatternExpr) -> usize {
    match pattern {
        PatternExpr::BindVar => 1,
        PatternExpr::Vec(v) => v.iter().map(|e| var_count(e)).sum(),
        PatternExpr::NumericConstant(_) => 0,
        PatternExpr::StringConstant(_) => 0,
        PatternExpr::BooleanConstant(_) => 0,
        PatternExpr::Unit => 0,
        PatternExpr::Bound(_) => 0,
        PatternExpr::Constructed {
            constructor: _,
            arguments,
        } => arguments.iter().map(|pattern| var_count(pattern)).sum(),
    }
}

fn check_no_recursion(expr: &Expr, level: usize) -> Result<(), ParseError> {
    match expr {
        Expr::Function {
            arity: _,
            body: _,
            env_map,
        } => {
            if env_map.contains(&level) {
                return Err(ParseError::CyclicDefinition);
            }
        }
        Expr::Bound(i) => {
            if *i == level {
                return Err(ParseError::CyclicDefinition);
            }
        }
        Expr::Apply {
            function,
            arguments,
        } => {
            check_no_recursion(function, level)?;
            for arg in arguments {
                check_no_recursion(arg, level)?;
            }
        }
        Expr::Let { value, body } => {
            check_no_recursion(value, level + 1)?;
            check_no_recursion(body, level + 1)?;
        }
        Expr::Vec(vec) => {
            for e in vec {
                check_no_recursion(e, level)?;
            }
        }
        Expr::If {
            condition,
            true_branch,
            false_branch,
        } => {
            check_no_recursion(condition, level)?;
            check_no_recursion(true_branch, level)?;
            check_no_recursion(false_branch, level)?;
        }
        Expr::Match { value, cases } => {
            check_no_recursion(value, level)?;
            for (pattern, body) in cases {
                check_no_recursion(body, level + var_count(pattern))?;
            }
        }
        Expr::NumericConstant(_) => {}
        Expr::StringConstant(_) => {}
        Expr::BooleanConstant(_) => {}
        Expr::Unit => {}
        Expr::Builtin(_) => {}
        Expr::TypeDeclaration { constructors, body } => {
            check_no_recursion(body, level + constructors.len())?;
        }
    }
    Ok(())
}

// Checks that no references to bound variable at index 0 exist which aren't guarded by a function
fn check_recursion_guarded(expr: &Expr, level: usize) -> Result<(), ParseError> {
    match expr {
        Expr::Bound(i) => {
            if *i == level {
                return Err(ParseError::CyclicDefinition);
            }
        }
        Expr::Apply {
            function,
            arguments,
        } => {
            check_no_recursion(function, level)?;
            for argument in arguments {
                check_no_recursion(argument, level)?;
            }
        }
        Expr::Let { value, body } => {
            check_no_recursion(value, level + 1)?;
            check_recursion_guarded(body, level + 1)?;
        }
        Expr::Vec(vec) => {
            for expr in vec {
                check_no_recursion(expr, level)?;
            }
        }
        Expr::If {
            condition,
            true_branch,
            false_branch,
        } => {
            check_no_recursion(condition, level)?;
            check_recursion_guarded(true_branch, level)?;
            check_recursion_guarded(false_branch, level)?;
        }
        Expr::Match { value, cases } => {
            check_no_recursion(value, level)?;
            for (pattern, body) in cases {
                check_recursion_guarded(body, level + var_count(pattern))?;
            }
        }
        Expr::NumericConstant(_) => {}
        Expr::StringConstant(_) => {}
        Expr::BooleanConstant(_) => {}
        Expr::Unit => {}
        Expr::Function { .. } => {}
        Expr::Builtin(_) => {}
        Expr::TypeDeclaration { constructors, body } => {
            check_recursion_guarded(body, level + constructors.len())?;
        }
    }
    Ok(())
}

fn compile_pattern<'a>(
    sexpr: &'a Located<Sexpr<Token>>,
    env: &mut VecDeque<&'a str>,
    vars: &mut Vec<&'a str>,
) -> Result<(Box<PatternExpr>, Vec<usize>), Located<ParseError>> {
    match &sexpr.value {
        Sexpr::Atom(Token::IdentifierBinding(s)) => {
            vars.push(s.as_deref().unwrap_or(""));
            result(PatternExpr::BindVar)
        }
        Sexpr::List(list) => {
            if list.len() < 1 {
                return Err(Located::new(
                    sexpr.source_range.clone(),
                    ParseError::InvalidSyntax,
                ));
            }
            let (first, rest) = list.split_first().unwrap();
            match &first.value {
                Sexpr::Atom(Token::Keyword(Keyword::Tuple)) => {
                    let mut patterns = Vec::with_capacity(rest.len());
                    let mut used_vars = vec![];
                    for sexpr in rest {
                        let (pattern, pattern_used_vars) = compile_pattern(sexpr, env, vars)?;
                        patterns.push(pattern);
                        used_vars = merge(&used_vars, &pattern_used_vars);
                    }
                    result_with_vars(PatternExpr::Vec(patterns), used_vars)
                }
                Sexpr::Atom(Token::Identifier(s)) => {
                    let constructor = match lookup(s, env) {
                        None => Err(Located::new(
                            first.source_range.clone(),
                            ParseError::UndefinedVariable,
                        )),
                        Some(i) => Ok(i),
                    }?;
                    let mut patterns = Vec::with_capacity(rest.len());
                    let mut used_vars = vec![constructor];
                    for sexpr in rest {
                        let (pattern, pattern_used_vars) = compile_pattern(sexpr, env, vars)?;
                        patterns.push(pattern);
                        used_vars = merge(&used_vars, &pattern_used_vars);
                    }
                    result_with_vars(
                        PatternExpr::Constructed {
                            constructor,
                            arguments: patterns,
                        },
                        used_vars,
                    )
                }
                _ => Err(Located::new(
                    sexpr.source_range.clone(),
                    ParseError::InvalidSyntax,
                )),
            }
        }
        _ => {
            let (expr, used_vars) = compile_sexpr(sexpr, env)?;
            let pattern = match *expr {
                Expr::NumericConstant(n) => Ok(PatternExpr::NumericConstant(n)),
                Expr::StringConstant(s) => Ok(PatternExpr::StringConstant(s)),
                Expr::BooleanConstant(b) => Ok(PatternExpr::BooleanConstant(b)),
                Expr::Unit => Ok(PatternExpr::Unit),
                Expr::Bound(i) => Ok(PatternExpr::Bound(i)),
                _ => Err(Located::new(
                    sexpr.source_range.clone(),
                    ParseError::InvalidExpressionInPattern,
                )),
            }?;
            result_with_vars(pattern, used_vars)
        }
    }
}

fn compile_case<'a>(
    pattern_sexpr: &'a Located<Sexpr<Token>>,
    body_sexpr: &'a Located<Sexpr<Token>>,
    env: &mut VecDeque<&'a str>,
) -> Result<(PatternExpr, Box<Expr>, Vec<usize>), Located<ParseError>> {
    let mut defined_vars = Vec::new();
    let (pattern, pattern_used_vars) = compile_pattern(pattern_sexpr, env, &mut defined_vars)?;
    let var_count = defined_vars.len();
    for var in defined_vars {
        env.push_front(var);
    }
    let (body, body_used_vars) = compile_sexpr(body_sexpr, env)?;
    for _ in 0..var_count {
        env.pop_front();
    }
    Ok((
        *pattern,
        body,
        merge(
            &pattern_used_vars,
            &parent_used_vars(body_used_vars, var_count),
        ),
    ))
}

fn compile_cases<'a>(
    source_range: &Range<usize>,
    sexprs: &'a [Located<Sexpr<Token>>],
    env: &mut VecDeque<&'a str>,
) -> Result<(Vec<(PatternExpr, Box<Expr>)>, Vec<usize>), Located<ParseError>> {
    if sexprs.len() % 2 != 0 {
        return Err(Located::new(
            source_range.clone(),
            ParseError::InvalidSyntax,
        ));
    }
    let mut used_vars = vec![];
    let mut cases = Vec::with_capacity(sexprs.len() / 2);
    for i in 0..(sexprs.len() / 2) {
        let (pattern_expr, body, case_used_vars) =
            compile_case(&sexprs[2 * i], &sexprs[2 * i + 1], env)?;
        cases.push((pattern_expr, body));
        used_vars = merge(&used_vars, &case_used_vars);
    }
    Ok((cases, used_vars))
}

fn compile_data_constructor<'a>(
    sexpr: &'a Located<Sexpr<Token>>,
    env: &mut VecDeque<&'a str>,
) -> Result<usize, Located<ParseError>> {
    match &sexpr.value {
        Sexpr::Atom(Token::IdentifierBinding(s)) => {
            env.push_front(s.as_deref().unwrap_or(""));
            Ok(0)
        }
        Sexpr::List(list) => {
            if list.len() < 2 {
                return Err(Located::new(
                    sexpr.source_range.clone(),
                    ParseError::InvalidSyntax,
                ));
            }
            if let Sexpr::Atom(Token::IdentifierBinding(s)) = &list[0].value {
                env.push_front(s.as_deref().unwrap_or(""));
            } else {
                return Err(Located::new(
                    list[0].source_range.clone(),
                    ParseError::InvalidSyntax,
                ));
            }
            for v in &list[1..list.len()] {
                if let Sexpr::Atom(Token::IdentifierBinding(None)) = v.value {
                } else {
                    return Err(Located::new(
                        v.source_range.clone(),
                        ParseError::InvalidFieldDeclaration,
                    ));
                }
            }
            Ok(list.len() - 1)
        }
        _ => Err(Located::new(
            sexpr.source_range.clone(),
            ParseError::UnexpectedToken,
        )),
    }
}

fn compile_form<'a>(
    source_range: &Range<usize>,
    head: Located<&Token>,
    rest: &'a [Located<Sexpr<Token>>],
    env: &mut VecDeque<&'a str>,
) -> Result<(Box<Expr>, Vec<usize>), Located<ParseError>> {
    match &head.value {
        Token::Keyword(Keyword::Function) => {
            if rest.len() < 1 {
                return Err(head.with_value(ParseError::InvalidSyntax));
            }
            let (body, args) = rest.split_last().unwrap();
            let vars = read_function_vars(args)?;
            for v in vars.iter().rev() {
                env.push_front(v);
            }
            let (expr, used_vars) = compile_sexpr(body, env)?;
            // Change de Bruijn indices for parent environment
            let parent_used_vars = parent_used_vars(used_vars, vars.len());
            for _ in &vars {
                env.pop_front();
            }
            result_with_vars(
                Expr::Function {
                    arity: vars.len(),
                    body: expr,
                    env_map: parent_used_vars.clone(),
                },
                parent_used_vars,
            )
        }
        Token::Keyword(Keyword::Let) => {
            if rest.len() != 3 {
                return Err(Located::new(
                    source_range.clone(),
                    ParseError::InvalidSyntax,
                ));
            }
            match &rest[0].value {
                Sexpr::Atom(Token::IdentifierBinding(s)) => {
                    let name = s.as_deref().unwrap_or("");
                    env.push_front(name);
                    let (value_expression, value_used_vars) = compile_sexpr(&rest[1], env)?;
                    check_recursion_guarded(&value_expression, 0)
                        .map_err(|e| Located::new(rest[0].source_range.clone(), e))?;
                    let (body_expression, body_used_vars) = compile_sexpr(&rest[2], env)?;
                    env.pop_front();
                    let parent_used_vars =
                        parent_used_vars(merge(&body_used_vars, &value_used_vars), 1);

                    result_with_vars(
                        Expr::Let {
                            value: value_expression,
                            body: body_expression,
                        },
                        parent_used_vars,
                    )
                }
                _ => Err(Located::new(
                    rest[0].source_range.clone(),
                    ParseError::InvalidVariableBinding,
                )),
            }
        }
        Token::Keyword(Keyword::If) => {
            if rest.len() != 3 {
                return Err(Located::new(
                    source_range.clone(),
                    ParseError::InvalidSyntax,
                ));
            }
            let (condition, condition_used_vars) = compile_sexpr(&rest[0], env)?;
            let (true_branch, true_used_vars) = compile_sexpr(&rest[1], env)?;
            let (false_branch, false_used_vars) = compile_sexpr(&rest[2], env)?;
            result_with_vars(
                Expr::If {
                    condition,
                    true_branch,
                    false_branch,
                },
                merge(
                    &condition_used_vars,
                    &merge(&true_used_vars, &false_used_vars),
                ),
            )
        }
        Token::Keyword(Keyword::Match) => {
            if rest.len() < 1 {
                return Err(Located::new(
                    source_range.clone(),
                    ParseError::InvalidSyntax,
                ));
            }
            let (value_sexpr, cases_sexprs) = rest.split_first().unwrap();
            let (value, value_used_vars) = compile_sexpr(value_sexpr, env)?;
            let (cases, cases_used_vars) = compile_cases(source_range, cases_sexprs, env)?;
            result_with_vars(
                Expr::Match { value, cases },
                merge(&value_used_vars, &cases_used_vars),
            )
        }
        Token::Keyword(Keyword::Data) => {
            if rest.len() < 2 {
                return Err(Located::new(
                    source_range.clone(),
                    ParseError::InvalidSyntax,
                ));
            }
            let mut var_counts = Vec::new();
            for sexpr in &rest[..rest.len() - 1] {
                var_counts.push(compile_data_constructor(sexpr, env)?);
            }
            let constructor_count = var_counts.len();
            let (body, body_used_vars) = compile_sexpr(&rest.last().unwrap(), env)?;
            for _ in 0..constructor_count {
                env.pop_front();
            }
            result_with_vars(
                Expr::TypeDeclaration {
                    constructors: var_counts,
                    body,
                },
                parent_used_vars(body_used_vars, constructor_count),
            )
        }
        v => apply(
            compile_atom(v, env).map_err(|e| Located::new(head.source_range, e))?,
            rest,
            env,
        ),
    }
}

fn apply<'a>(
    function: (Box<Expr>, Vec<usize>),
    rest: &'a [Located<Sexpr<Token>>],
    env: &mut VecDeque<&'a str>,
) -> Result<(Box<Expr>, Vec<usize>), Located<ParseError>> {
    let (function_expr, mut used_vars) = function;
    let mut arguments = Vec::new();
    for sexpr in rest {
        let (arg_expr, arg_used_vars) = compile_sexpr(sexpr, env)?;
        used_vars = merge(&used_vars, &arg_used_vars);
        arguments.push(arg_expr);
    }
    result_with_vars(
        Expr::Apply {
            function: function_expr,
            arguments,
        },
        used_vars,
    )
}

fn compile_sexpr<'a>(
    sexpr: &'a Located<Sexpr<Token>>,
    env: &mut VecDeque<&'a str>,
) -> Result<(Box<Expr>, Vec<usize>), Located<ParseError>> {
    match &sexpr.value {
        Sexpr::Atom(token) => {
            compile_atom(token, env).map_err(|e| Located::new(sexpr.source_range.clone(), e))
        }
        Sexpr::List(list) => match list.split_first() {
            None => result(Expr::Unit),
            Some((head, rest)) => match &head.value {
                Sexpr::Atom(Token::Keyword(Keyword::Tuple)) => {
                    let mut result = Vec::new();
                    let mut used_vars = Vec::new();
                    for sexpr in rest {
                        let (expr, new_used_vars) = compile_sexpr(sexpr, env)?;
                        result.push(expr);
                        used_vars = merge(&used_vars, &new_used_vars);
                    }
                    result_with_vars(Expr::Vec(result), used_vars)
                }
                Sexpr::Atom(token) => {
                    compile_form(&sexpr.source_range, head.with_value(token), rest, env)
                }
                Sexpr::List(_) => apply(compile_sexpr(head, env)?, rest, env),
            },
        },
    }
}

fn rewrite_env_map_pattern(pattern_expr: PatternExpr, current_env_map: &[usize]) -> PatternExpr {
    match pattern_expr {
        PatternExpr::Bound(i) => {
            PatternExpr::Bound(current_env_map.iter().position(|j| i == *j).unwrap())
        }
        PatternExpr::Vec(vec) => PatternExpr::Vec(
            vec.into_iter()
                .map(|e| Box::new(rewrite_env_map_pattern(*e, current_env_map)))
                .collect(),
        ),
        PatternExpr::BindVar => pattern_expr,
        PatternExpr::NumericConstant(_) => pattern_expr,
        PatternExpr::StringConstant(_) => pattern_expr,
        PatternExpr::BooleanConstant(_) => pattern_expr,
        PatternExpr::Unit => pattern_expr,
        PatternExpr::Constructed {
            constructor,
            arguments,
        } => PatternExpr::Constructed {
            constructor: current_env_map
                .iter()
                .position(|j| constructor == *j)
                .unwrap(),
            arguments: arguments
                .into_iter()
                .map(|e| Box::new(rewrite_env_map_pattern(*e, current_env_map)))
                .collect(),
        },
    }
}

/**
 * The original env maps we create are relative to all variables which are in scope. We want to
 * make them to be relative to the variables which are captured by the parent lambda only, since we
 * copy from the parent environment when creating a closure.
 *
 * We also need to rewrite bound variable indices to be relative to this new reduced environment.
 */
fn rewrite_env_maps(expr: Expr, current_env_map: &[usize]) -> Box<Expr> {
    Box::new(match expr {
        Expr::Function {
            arity,
            body,
            env_map,
        } => {
            let new_env_map: Vec<usize> = env_map
                .iter()
                .map(|i| current_env_map.iter().position(|j| *i == *j).unwrap())
                .collect();
            let child_env_map: Vec<usize> = (0..arity)
                .chain(new_env_map.iter().map(|i| arity + current_env_map[*i]))
                .collect();
            Expr::Function {
                arity,
                body: rewrite_env_maps(*body, &child_env_map),
                env_map: new_env_map,
            }
        }
        Expr::Bound(i) => Expr::Bound(
            current_env_map
                .iter()
                .position(|j| i == *j)
                .expect("Couldn't find bound variable"),
        ),
        Expr::Apply {
            function,
            arguments,
        } => Expr::Apply {
            function: rewrite_env_maps(*function, current_env_map),
            arguments: arguments
                .into_iter()
                .map(|argument| rewrite_env_maps(*argument, current_env_map))
                .collect(),
        },
        Expr::Let { value, body } => {
            let child_env_map: Vec<usize> = once(0)
                .chain(current_env_map.iter().map(|x| x + 1))
                .collect();
            Expr::Let {
                value: rewrite_env_maps(*value, &child_env_map),
                body: rewrite_env_maps(*body, &child_env_map),
            }
        }
        Expr::Vec(vec) => Expr::Vec(
            vec.into_iter()
                .map(|e| rewrite_env_maps(*e, current_env_map))
                .collect(),
        ),
        Expr::If {
            condition,
            true_branch,
            false_branch,
        } => Expr::If {
            condition: rewrite_env_maps(*condition, current_env_map),
            true_branch: rewrite_env_maps(*true_branch, current_env_map),
            false_branch: rewrite_env_maps(*false_branch, current_env_map),
        },
        Expr::Match { value, cases } => Expr::Match {
            value: rewrite_env_maps(*value, current_env_map),
            cases: cases
                .into_iter()
                .map(|(pattern, expr)| {
                    let var_count = var_count(&pattern);
                    let child_env_map: Vec<usize> = (0..var_count)
                        .chain(current_env_map.iter().map(|i| var_count + i))
                        .collect();
                    (
                        rewrite_env_map_pattern(pattern, current_env_map),
                        rewrite_env_maps(*expr, &child_env_map),
                    )
                })
                .collect(),
        },
        Expr::NumericConstant(_) => expr,
        Expr::StringConstant(_) => expr,
        Expr::BooleanConstant(_) => expr,
        Expr::Unit => expr,
        Expr::Builtin(_) => expr,
        Expr::TypeDeclaration { constructors, body } => {
            let child_env_map: Vec<usize> = (0..constructors.len())
                .chain(current_env_map.iter().map(|i| constructors.len() + i))
                .collect();
            Expr::TypeDeclaration {
                constructors,
                body: rewrite_env_maps(*body, &child_env_map),
            }
        }
    })
}

fn strip_comments(sexpr: Located<Sexpr<Token>>) -> Option<Located<Sexpr<Token>>> {
    match sexpr.value {
        Sexpr::Atom(Token::Comment) => None,
        Sexpr::Atom(_) => Some(sexpr),
        Sexpr::List(list) => Some(Located::new(
            sexpr.source_range,
            Sexpr::List(list.into_iter().flat_map(|e| strip_comments(e)).collect()),
        )),
    }
}

pub fn compile(s: &str) -> Result<Expr, Located<ParseError>> {
    let sexpr = parse(
        s,
        |s, quoted| {
            Token::try_from(if quoted {
                unescape_string(s).unwrap()
            } else {
                s.to_string()
            })
            .map_err(|e| ParseError::InvalidToken(e))
        },
        ParseError::UnexpectedCharacter,
    )?;
    let sexpr_no_comments = strip_comments(sexpr).unwrap();
    match sexpr_no_comments.value {
        Sexpr::Atom(_) => panic!("Expected list"),
        Sexpr::List(list) => {
            if list.is_empty() {
                return Err(Located::new(
                    sexpr_no_comments.source_range.clone(),
                    ParseError::EmptyInput,
                ));
            }
            if list.len() > 1 {
                return Err(Located::new(
                    list[1].source_range.clone(),
                    ParseError::UnexpectedToken,
                ));
            }
            let (expr, used_vars) = compile_sexpr(&list[0], &mut VecDeque::new())?;
            if !used_vars.is_empty() {
                panic!("Vars referenced that don't exist, should be impossible");
            }
            Ok(*rewrite_env_maps(*expr, &[]))
        }
    }
}
