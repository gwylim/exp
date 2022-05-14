use crate::builtin::{get_builtin, Builtin};
use crate::program::ParseError::InvalidVariableBinding;
use crate::sexpr::{Sexpr, SexprBody};
use std::collections::vec_deque::VecDeque;
use std::iter::once;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedOperator(char),
    InvalidVariableBinding,
    CyclicDefinition(String),
    UndefinedVariable(String),
    InvalidExpressionInPattern,
    InvalidSyntax,
    InvalidNumericLiteral(String),
    // Intending to support named fields, but for now they must always be _
    InvalidFieldDeclaration,
}

// An expression together with indices of variables which it uses from its environment
type ParseResult<O> = Result<O, ParseError>;

type ExprResult<E> = ParseResult<(Box<E>, Vec<usize>)>;

fn result<E>(expr: E) -> ExprResult<E> {
    Ok((Box::new(expr), Vec::new()))
}

fn result_with_vars<E>(expr: E, used_vars: Vec<usize>) -> ExprResult<E> {
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

fn compile_atom(s: &str, operator: Option<&char>, env: &VecDeque<&str>) -> ExprResult<Expr> {
    match operator {
        Some(&'"') => result(Expr::StringConstant(s.to_string())),
        None => {
            let first: char = s.chars().nth(0).unwrap();
            if first.is_numeric() || first == '-' {
                s.parse()
                    .map(|value| result(Expr::NumericConstant(value)))
                    .unwrap_or(Err(ParseError::InvalidNumericLiteral(s.to_string())))
            } else {
                if s == "true" {
                    return result(Expr::BooleanConstant(true));
                }
                if s == "false" {
                    return result(Expr::BooleanConstant(false));
                }
                match lookup(s, env) {
                    None => get_builtin(s)
                        .map(|b| result(Expr::Builtin(b)))
                        .unwrap_or(Err(ParseError::UndefinedVariable(s.to_string()))),
                    Some(i) => result_with_vars(Expr::Bound(i), vec![i]),
                }
            }
        }
        Some(c) => Err(ParseError::UnexpectedOperator(*c)),
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

fn read_function_vars(list: &[Sexpr]) -> ParseResult<Vec<&str>> {
    list.iter()
        .map(|sexpr| match sexpr {
            Sexpr {
                contents: SexprBody::Atom(s),
                operator: Some('$'),
                comment: _,
            } => Ok(s.as_ref()),
            _ => Err(InvalidVariableBinding),
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

fn check_no_recursion(name: &str, expr: &Expr, level: usize) -> ParseResult<()> {
    match expr {
        Expr::Function {
            arity: _,
            body: _,
            env_map,
        } => {
            if env_map.contains(&level) {
                return Err(ParseError::CyclicDefinition(name.to_string()));
            }
        }
        Expr::Bound(i) => {
            if *i == level {
                return Err(ParseError::CyclicDefinition(name.to_string()));
            }
        }
        Expr::Apply {
            function,
            arguments,
        } => {
            check_no_recursion(name, function, level)?;
            for arg in arguments {
                check_no_recursion(name, arg, level)?;
            }
        }
        Expr::Let { value, body } => {
            check_no_recursion(name, value, level + 1)?;
            check_no_recursion(name, body, level + 1)?;
        }
        Expr::Vec(vec) => {
            for e in vec {
                check_no_recursion(name, e, level)?;
            }
        }
        Expr::If {
            condition,
            true_branch,
            false_branch,
        } => {
            check_no_recursion(name, condition, level)?;
            check_no_recursion(name, true_branch, level)?;
            check_no_recursion(name, false_branch, level)?;
        }
        Expr::Match { value, cases } => {
            check_no_recursion(name, value, level)?;
            for (pattern, body) in cases {
                check_no_recursion(name, body, level + var_count(pattern))?;
            }
        }
        Expr::NumericConstant(_) => {}
        Expr::StringConstant(_) => {}
        Expr::BooleanConstant(_) => {}
        Expr::Unit => {}
        Expr::Builtin(_) => {}
        Expr::TypeDeclaration { constructors, body } => {
            check_no_recursion(name, body, level + constructors.len())?;
        }
    }
    Ok(())
}

// Checks that no references to bound variable at index 0 exist which aren't guarded by a function
fn check_recursion_guarded(name: &str, expr: &Expr, level: usize) -> ParseResult<()> {
    match expr {
        Expr::Bound(i) => {
            if *i == level {
                return Err(ParseError::CyclicDefinition(name.to_string()));
            }
        }
        Expr::Apply {
            function,
            arguments,
        } => {
            check_no_recursion(name, function, level)?;
            for argument in arguments {
                check_no_recursion(name, argument, level)?;
            }
        }
        Expr::Let { value, body } => {
            check_no_recursion(name, value, level + 1)?;
            check_recursion_guarded(name, body, level + 1)?;
        }
        Expr::Vec(vec) => {
            for expr in vec {
                check_no_recursion(name, expr, level)?;
            }
        }
        Expr::If {
            condition,
            true_branch,
            false_branch,
        } => {
            check_no_recursion(name, condition, level)?;
            check_recursion_guarded(name, true_branch, level)?;
            check_recursion_guarded(name, false_branch, level)?;
        }
        Expr::Match { value, cases } => {
            check_no_recursion(name, value, level)?;
            for (pattern, body) in cases {
                check_recursion_guarded(name, body, level + var_count(pattern))?;
            }
        }
        Expr::NumericConstant(_) => {}
        Expr::StringConstant(_) => {}
        Expr::BooleanConstant(_) => {}
        Expr::Unit => {}
        Expr::Function { .. } => {}
        Expr::Builtin(_) => {}
        Expr::TypeDeclaration { constructors, body } => {
            check_recursion_guarded(name, body, level + constructors.len())?;
        }
    }
    Ok(())
}

fn compile_pattern<'a>(
    sexpr: &'a Sexpr,
    env: &mut VecDeque<&'a str>,
    vars: &mut Vec<&'a str>,
) -> ExprResult<PatternExpr> {
    match sexpr {
        Sexpr {
            contents: SexprBody::Atom(s),
            operator: Some('$'),
            comment: _,
        } => {
            vars.push(s);
            result(PatternExpr::BindVar)
        }
        Sexpr {
            contents: SexprBody::List(v),
            operator: Some('#'),
            comment: _,
        } => {
            let mut patterns = Vec::with_capacity(v.len());
            let mut used_vars = vec![];
            for sexpr in v {
                let (pattern, pattern_used_vars) = compile_pattern(sexpr, env, vars)?;
                patterns.push(pattern);
                used_vars = merge(&used_vars, &pattern_used_vars);
            }
            result_with_vars(PatternExpr::Vec(patterns), used_vars)
        }
        Sexpr {
            contents: SexprBody::List(v),
            operator: None,
            comment: _,
        } => {
            if v.len() < 1 {
                return Err(ParseError::InvalidSyntax);
            }
            let constructor = match &v[0] {
                Sexpr {
                    contents: SexprBody::Atom(s),
                    operator: None,
                    comment: _,
                } => match lookup(s, env) {
                    None => {
                        return Err(ParseError::UndefinedVariable(s.to_string()));
                    }
                    Some(i) => i,
                },
                _ => return Err(ParseError::InvalidSyntax),
            };
            let mut patterns = Vec::with_capacity(v.len());
            let mut used_vars = vec![constructor];
            for sexpr in &v[1..] {
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
        _ => {
            let (expr, used_vars) = compile_sexpr(sexpr, env)?;
            let pattern = match *expr {
                Expr::NumericConstant(n) => Ok(PatternExpr::NumericConstant(n)),
                Expr::StringConstant(s) => Ok(PatternExpr::StringConstant(s)),
                Expr::BooleanConstant(b) => Ok(PatternExpr::BooleanConstant(b)),
                Expr::Unit => Ok(PatternExpr::Unit),
                Expr::Bound(i) => Ok(PatternExpr::Bound(i)),
                _ => Err(ParseError::InvalidExpressionInPattern),
            }?;
            result_with_vars(pattern, used_vars)
        }
    }
}

fn compile_case<'a>(
    pattern_sexpr: &'a Sexpr,
    body_sexpr: &'a Sexpr,
    env: &mut VecDeque<&'a str>,
) -> ParseResult<(PatternExpr, Box<Expr>, Vec<usize>)> {
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
    sexprs: &'a [Sexpr],
    env: &mut VecDeque<&'a str>,
) -> ParseResult<(Vec<(PatternExpr, Box<Expr>)>, Vec<usize>)> {
    if sexprs.len() % 2 != 0 {
        return Err(ParseError::InvalidSyntax);
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
    sexpr: &'a Sexpr,
    env: &mut VecDeque<&'a str>,
) -> ParseResult<usize> {
    match sexpr {
        Sexpr {
            comment: _,
            operator: Some('$'),
            contents: SexprBody::Atom(s),
        } => {
            env.push_front(s);
            Ok(0)
        }
        Sexpr {
            comment: _,
            operator: None,
            contents: SexprBody::List(vec),
        } => {
            if vec.len() < 2 {
                return Err(ParseError::InvalidSyntax);
            }
            if let Sexpr {
                contents: SexprBody::Atom(s),
                operator: Some('$'),
                comment: _,
            } = &vec[0]
            {
                env.push_front(s);
            } else {
                return Err(ParseError::InvalidSyntax);
            }
            for v in &vec[1..vec.len()] {
                if let Sexpr {
                    comment: _,
                    operator: None,
                    contents: SexprBody::Atom(s),
                } = v
                {
                    if s != "_" {
                        return Err(ParseError::InvalidFieldDeclaration);
                    }
                } else {
                    return Err(ParseError::InvalidFieldDeclaration);
                }
            }
            Ok(vec.len() - 1)
        }
        _ => Err(ParseError::InvalidSyntax),
    }
}

fn compile_form<'a>(
    head: &str,
    rest: &'a [Sexpr],
    env: &mut VecDeque<&'a str>,
) -> ExprResult<Expr> {
    match head {
        "fn" => {
            if rest.len() < 1 {
                return Err(ParseError::InvalidSyntax);
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
        "let" => {
            if rest.len() != 3 {
                return Err(ParseError::InvalidSyntax);
            }
            match &rest[0] {
                Sexpr {
                    contents: SexprBody::Atom(s),
                    operator: Some('$'),
                    comment: _,
                } => {
                    env.push_front(s);
                    let (value_expression, value_used_vars) = compile_sexpr(&rest[1], env)?;
                    check_recursion_guarded(s, &value_expression, 0)?;
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
                _ => Err(ParseError::InvalidVariableBinding),
            }
        }
        "if" => {
            if rest.len() != 3 {
                return Err(ParseError::InvalidSyntax);
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
        "match" => {
            if rest.len() < 1 {
                return Err(ParseError::InvalidSyntax);
            }
            let (value_sexpr, cases_sexprs) = rest.split_first().unwrap();
            let (value, value_used_vars) = compile_sexpr(value_sexpr, env)?;
            let (cases, cases_used_vars) = compile_cases(cases_sexprs, env)?;
            result_with_vars(
                Expr::Match { value, cases },
                merge(&value_used_vars, &cases_used_vars),
            )
        }
        "data" => {
            if rest.len() < 2 {
                return Err(ParseError::InvalidSyntax);
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
        v => apply(compile_atom(v, None, env)?, rest, env),
    }
}

fn apply<'a>(
    function: (Box<Expr>, Vec<usize>),
    rest: &'a [Sexpr],
    env: &mut VecDeque<&'a str>,
) -> ExprResult<Expr> {
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

fn compile_sexpr<'a>(sexpr: &'a Sexpr, env: &mut VecDeque<&'a str>) -> ExprResult<Expr> {
    match &sexpr {
        Sexpr {
            contents: SexprBody::Atom(s),
            operator,
            comment: _,
        } => compile_atom(s, operator.as_ref(), env),
        Sexpr {
            contents: SexprBody::List(list),
            operator: None,
            comment: _,
        } => match list.split_first() {
            None => result(Expr::Unit),
            Some((head, rest)) => match head {
                Sexpr {
                    contents: SexprBody::Atom(head),
                    operator: None,
                    comment: _,
                } => compile_form(head, rest, env),
                Sexpr {
                    contents: SexprBody::List(_),
                    operator: None,
                    comment: _,
                } => apply(compile_sexpr(head, env)?, rest, env),
                _ => Err(ParseError::UnexpectedOperator(head.operator.unwrap())),
            },
        },
        Sexpr {
            contents: SexprBody::List(list),
            operator: Some('#'),
            comment: _,
        } => {
            let mut result = Vec::new();
            let mut used_vars = Vec::new();
            for sexpr in list {
                let (expr, new_used_vars) = compile_sexpr(sexpr, env)?;
                result.push(expr);
                used_vars = merge(&used_vars, &new_used_vars);
            }
            result_with_vars(Expr::Vec(result), used_vars)
        }
        _ => Err(ParseError::UnexpectedOperator(sexpr.operator.unwrap())),
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

pub fn compile(sexpr: &Sexpr) -> ParseResult<Expr> {
    let (expr, used_vars) = compile_sexpr(sexpr, &mut VecDeque::new())?;
    if !used_vars.is_empty() {
        panic!("Vars referenced that don't exist, should be impossible");
    }
    Ok(*rewrite_env_maps(*expr, &[]))
}
