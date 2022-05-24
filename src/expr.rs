use crate::builtin::{get_builtin, Builtin};
use crate::located::Located;
use crate::sexpr::{unescape_string, Sexpr};
use crate::token::{Keyword, Token};
use crate::value::VecType;
use crate::{parse, token, InvalidTokenError};
use std::collections::vec_deque::VecDeque;
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
    NumericConstant(f64),
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
        Token::StringLiteral(s) => result(Expr::StringConstant(s.to_string())),
        Token::NumericLiteral(x) => result(Expr::NumericConstant(*x)),
        Token::BooleanLiteral(b) => result(Expr::BooleanConstant(*b)),
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
    list: &[Located<Sexpr<Token<String>>>],
) -> Result<Vec<Option<&str>>, Located<ParseError>> {
    list.iter()
        .map(|sexpr| match &sexpr.value {
            Sexpr::Atom(Token::IdentifierBinding(s)) => Ok(s.as_deref()),
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
        PatternExpr::Vec {
            values,
            vec_type: _,
        } => values.iter().map(|e| var_count(e)).sum(),
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

fn check_no_recursion_in_decls(
    decls: &Vec<Declaration>,
    level: usize,
) -> Result<usize, ParseError> {
    let mut var_count = 0;
    for decl in decls {
        match decl {
            Declaration::Let(value) => {
                var_count += 1;
                check_no_recursion(value, level + var_count)?;
            }
            Declaration::Type(constructors) => {
                var_count += constructors.len();
            }
        }
    }
    Ok(var_count)
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
        Expr::Declarations { decls, body } => {
            let var_count = check_no_recursion_in_decls(decls, level)?;
            check_no_recursion(body, level + var_count)?;
        }
        Expr::Vec {
            values,
            vec_type: _,
        } => {
            for e in values {
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
        Expr::Loop { initial_vars, body } => {
            for var in initial_vars {
                check_no_recursion(var, level)?;
            }
            check_no_recursion(body, level + initial_vars.len())?;
        }
        Expr::Next(arguments) => {
            for arg in arguments {
                check_no_recursion(arg, level)?;
            }
        }
        Expr::NumericConstant(_) => {}
        Expr::StringConstant(_) => {}
        Expr::BooleanConstant(_) => {}
        Expr::Unit => {}
        Expr::Builtin(_) => {}
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
        Expr::Declarations { decls, body } => {
            let var_count = check_no_recursion_in_decls(decls, level)?;
            check_recursion_guarded(body, level + var_count)?;
        }
        Expr::Vec {
            values,
            vec_type: _,
        } => {
            for expr in values {
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
        Expr::Loop { initial_vars, body } => {
            for var in initial_vars {
                check_no_recursion(var, level)?;
            }
            check_recursion_guarded(body, level + initial_vars.len())?;
        }
        Expr::Next(arguments) => {
            for arg in arguments {
                check_no_recursion(arg, level)?;
            }
        }
        Expr::NumericConstant(_) => {}
        Expr::StringConstant(_) => {}
        Expr::BooleanConstant(_) => {}
        Expr::Unit => {}
        Expr::Function { .. } => {}
        Expr::Builtin(_) => {}
    }
    Ok(())
}

fn compile_pattern<'a>(
    sexpr: &'a Located<Sexpr<Token<String>>>,
    env: &mut VecDeque<Option<&'a str>>,
    vars: &mut Vec<Option<&'a str>>,
) -> Result<(PatternExpr, Vec<usize>), Located<ParseError>> {
    match &sexpr.value {
        Sexpr::Atom(Token::IdentifierBinding(s)) => {
            vars.push(s.as_deref());
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
                Sexpr::Atom(Token::Keyword(Keyword::Tuple))
                | Sexpr::Atom(Token::Keyword(Keyword::Array)) => {
                    let mut patterns = Vec::with_capacity(rest.len());
                    let mut used_vars = vec![];
                    for sexpr in rest {
                        let (pattern, pattern_used_vars) = compile_pattern(sexpr, env, vars)?;
                        patterns.push(pattern);
                        used_vars = merge(&used_vars, &pattern_used_vars);
                    }
                    let vec_type = match &first.value {
                        Sexpr::Atom(Token::Keyword(k)) => match k {
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
                    Ok((
                        PatternExpr::Constructed {
                            constructor,
                            arguments: patterns,
                        },
                        used_vars,
                    ))
                }
                _ => Err(Located::new(
                    sexpr.source_range.clone(),
                    ParseError::InvalidSyntax,
                )),
            }
        }
        _ => {
            let (expr, used_vars) = compile_sexpr(sexpr, env)?;
            let pattern = match expr {
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
            Ok((pattern, used_vars))
        }
    }
}

fn compile_case<'a>(
    pattern_sexpr: &'a Located<Sexpr<Token<String>>>,
    body_sexpr: &'a Located<Sexpr<Token<String>>>,
    env: &mut VecDeque<Option<&'a str>>,
) -> Result<(PatternExpr, Expr, Vec<usize>), Located<ParseError>> {
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
        pattern,
        body,
        merge(
            &pattern_used_vars,
            &parent_used_vars(body_used_vars, var_count),
        ),
    ))
}

fn compile_cases<'a>(
    source_range: &Range<usize>,
    sexprs: &'a [Located<Sexpr<Token<String>>>],
    env: &mut VecDeque<Option<&'a str>>,
) -> Result<(Vec<(PatternExpr, Expr)>, Vec<usize>), Located<ParseError>> {
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
    sexpr: &'a Located<Sexpr<Token<String>>>,
    env: &mut VecDeque<Option<&'a str>>,
) -> Result<usize, Located<ParseError>> {
    match &sexpr.value {
        Sexpr::Atom(Token::IdentifierBinding(s)) => {
            env.push_front(s.as_deref());
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
                env.push_front(s.as_deref());
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

fn apply<'a>(
    function: (Expr, Vec<usize>),
    rest: &'a [Located<Sexpr<Token<String>>>],
    env: &mut VecDeque<Option<&'a str>>,
) -> Result<(Expr, Vec<usize>), Located<ParseError>> {
    let (function_expr, mut used_vars) = function;
    let mut arguments = Vec::new();
    for sexpr in rest {
        let (arg_expr, arg_used_vars) = compile_sexpr(sexpr, env)?;
        used_vars = merge(&used_vars, &arg_used_vars);
        arguments.push(arg_expr);
    }
    Ok((
        Expr::Apply {
            function: Box::new(function_expr),
            arguments,
        },
        used_vars,
    ))
}

fn compile_loop_var<'a>(
    sexpr: &'a Located<Sexpr<Token<String>>>,
    env: &mut VecDeque<Option<&'a str>>,
) -> Result<(Option<&'a str>, Expr, Vec<usize>), Located<ParseError>> {
    match &sexpr.value {
        Sexpr::Atom(_) => Err(sexpr.with_value(ParseError::InvalidSyntax)),
        Sexpr::List(list) => {
            if list.len() != 2 {
                return Err(sexpr.with_value(ParseError::InvalidSyntax));
            }
            match &list[0].value {
                Sexpr::Atom(Token::IdentifierBinding(s)) => {
                    let (value, value_used_vars) = compile_sexpr(&list[1], env)?;
                    Ok((s.as_deref(), value, value_used_vars))
                }
                _ => Err(list[0].with_value(ParseError::InvalidVariableBinding)),
            }
        }
    }
}

fn compile_sexpr<'a>(
    mut sexpr: &'a Located<Sexpr<Token<String>>>,
    env: &mut VecDeque<Option<&'a str>>,
) -> Result<(Expr, Vec<usize>), Located<ParseError>> {
    let mut decls = Vec::new();
    let mut var_count = 0;
    // Variables used from the parent environment (i.e. from the initial value of env)
    let mut used_vars = Vec::new();
    let body = loop {
        match &sexpr.value {
            Sexpr::Atom(token) => {
                let (expr, expr_used_vars) = compile_atom(token, env)
                    .map_err(|e| Located::new(sexpr.source_range.clone(), e))?;
                used_vars = merge(&used_vars, &parent_used_vars(expr_used_vars, var_count));
                break expr;
            }
            Sexpr::List(list) => match list.split_first() {
                None => break Expr::Unit,
                Some((head, rest)) => match &head.value {
                    Sexpr::Atom(token) => {
                        match token {
                            Token::Keyword(Keyword::Tuple) | Token::Keyword(Keyword::Array) => {
                                let mut values = Vec::new();
                                for sexpr in rest {
                                    let (expr, new_used_vars) = compile_sexpr(sexpr, env)?;
                                    values.push(expr);
                                    used_vars = merge(
                                        &used_vars,
                                        &parent_used_vars(new_used_vars, var_count),
                                    );
                                }
                                break Expr::Vec {
                                    values,
                                    vec_type: match token {
                                        Token::Keyword(Keyword::Tuple) => VecType::Tuple,
                                        Token::Keyword(Keyword::Array) => VecType::Array,
                                        _ => unreachable!(),
                                    },
                                };
                            }
                            Token::Keyword(Keyword::Function) => {
                                if rest.len() < 1 {
                                    return Err(head.with_value(ParseError::InvalidSyntax));
                                }
                                let (body, args) = rest.split_last().unwrap();
                                let vars = read_function_vars(args)?;
                                for v in vars.iter().rev() {
                                    env.push_front(*v);
                                }
                                var_count += vars.len();
                                let (expr, expr_used_vars) = compile_sexpr(body, env)?;
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
                                    env_map: parent_used_vars,
                                };
                            }
                            Token::Keyword(Keyword::Loop) => {
                                if rest.len() < 1 {
                                    return Err(sexpr.with_value(ParseError::InvalidSyntax));
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
                                let (body, body_used_vars) =
                                    compile_sexpr(&rest[rest.len() - 1], env)?;
                                used_vars =
                                    merge(&used_vars, &parent_used_vars(body_used_vars, var_count));
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
                                for sexpr in rest {
                                    let (expr, expr_used_vars) = compile_sexpr(sexpr, env)?;
                                    used_vars = merge(
                                        &used_vars,
                                        &parent_used_vars(expr_used_vars, var_count),
                                    );
                                    arguments.push(expr);
                                }
                                break Expr::Next(arguments);
                            }
                            Token::Keyword(Keyword::Let) => {
                                if rest.len() != 3 {
                                    return Err(Located::new(
                                        sexpr.source_range.clone(),
                                        ParseError::InvalidSyntax,
                                    ));
                                }
                                match &rest[0].value {
                                    Sexpr::Atom(Token::IdentifierBinding(s)) => {
                                        env.push_front(s.as_deref());
                                        var_count += 1;
                                        let (value_expression, value_used_vars) =
                                            compile_sexpr(&rest[1], env)?;
                                        check_recursion_guarded(&value_expression, 0).map_err(
                                            |e| Located::new(rest[0].source_range.clone(), e),
                                        )?;
                                        sexpr = &rest[2];
                                        used_vars = merge(
                                            &used_vars,
                                            &parent_used_vars(value_used_vars, var_count),
                                        );

                                        // FIXME: maybe we shouldn't return Box<Expr> from compile_*
                                        // functions?
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
                                        sexpr.source_range.clone(),
                                        ParseError::InvalidSyntax,
                                    ));
                                }
                                let (condition, condition_used_vars) =
                                    compile_sexpr(&rest[0], env)?;
                                let (true_branch, true_used_vars) = compile_sexpr(&rest[1], env)?;
                                let (false_branch, false_used_vars) = compile_sexpr(&rest[2], env)?;
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
                                        sexpr.source_range.clone(),
                                        ParseError::InvalidSyntax,
                                    ));
                                }
                                let (value_sexpr, cases_sexprs) = rest.split_first().unwrap();
                                let (value, value_used_vars) = compile_sexpr(value_sexpr, env)?;
                                let (cases, cases_used_vars) =
                                    compile_cases(&sexpr.source_range, cases_sexprs, env)?;
                                used_vars = merge(
                                    &used_vars,
                                    &parent_used_vars(
                                        merge(&value_used_vars, &cases_used_vars),
                                        var_count,
                                    ),
                                );
                                break Expr::Match {
                                    value: Box::new(value),
                                    cases,
                                };
                            }
                            Token::Keyword(Keyword::Data) => {
                                if rest.len() < 2 {
                                    return Err(Located::new(
                                        sexpr.source_range.clone(),
                                        ParseError::InvalidSyntax,
                                    ));
                                }
                                let mut constructor_var_counts = Vec::new();
                                for sexpr in &rest[..rest.len() - 1] {
                                    constructor_var_counts
                                        .push(compile_data_constructor(sexpr, env)?);
                                }
                                let constructor_count = constructor_var_counts.len();
                                var_count += constructor_count;
                                sexpr = &rest.last().unwrap();
                                decls.push(Declaration::Type(constructor_var_counts));
                            }
                            v => {
                                let (expr, expr_used_vars) = apply(
                                    compile_atom(v, env)
                                        .map_err(|e| Located::new(head.source_range.clone(), e))?,
                                    rest,
                                    env,
                                )?;
                                used_vars =
                                    merge(&used_vars, &parent_used_vars(expr_used_vars, var_count));
                                break expr;
                            }
                        }
                    }
                    Sexpr::List(_) => {
                        let (expr, expr_used_vars) = apply(compile_sexpr(head, env)?, rest, env)?;
                        used_vars = merge(&used_vars, &expr_used_vars);
                        break expr;
                    }
                },
            },
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

fn rewrite_env_map_pattern(pattern_expr: PatternExpr, current_env_map: &[usize]) -> PatternExpr {
    match pattern_expr {
        PatternExpr::Bound(i) => {
            PatternExpr::Bound(current_env_map.iter().position(|j| i == *j).unwrap())
        }
        PatternExpr::Vec { values, vec_type } => PatternExpr::Vec {
            values: values
                .into_iter()
                .map(|e| rewrite_env_map_pattern(e, current_env_map))
                .collect(),
            vec_type,
        },
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
                .map(|e| rewrite_env_map_pattern(e, current_env_map))
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
fn rewrite_env_maps(expr: Expr, current_env_map: &[usize]) -> Expr {
    match expr {
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
                body: Box::new(rewrite_env_maps(*body, &child_env_map)),
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
            function: Box::new(rewrite_env_maps(*function, current_env_map)),
            arguments: arguments
                .into_iter()
                .map(|argument| rewrite_env_maps(argument, current_env_map))
                .collect(),
        },
        Expr::Declarations { decls, body } => {
            let mut child_env_map: Vec<usize> = current_env_map.to_vec();
            let mut new_decls = Vec::new();
            for decl in decls {
                match decl {
                    Declaration::Let(expr) => {
                        child_env_map =
                            once(0).chain(child_env_map.iter().map(|x| x + 1)).collect();
                        new_decls.push(Declaration::Let(rewrite_env_maps(expr, &child_env_map)));
                    }
                    Declaration::Type(constructors) => {
                        child_env_map = (0..constructors.len())
                            .chain(child_env_map.iter().map(|i| constructors.len() + i))
                            .collect();
                        new_decls.push(Declaration::Type(constructors));
                    }
                }
            }
            Expr::Declarations {
                decls: new_decls,
                body: Box::new(rewrite_env_maps(*body, &child_env_map)),
            }
        }
        Expr::Vec { values, vec_type } => Expr::Vec {
            values: values
                .into_iter()
                .map(|e| rewrite_env_maps(e, current_env_map))
                .collect(),
            vec_type,
        },
        Expr::If {
            condition,
            true_branch,
            false_branch,
        } => Expr::If {
            condition: Box::new(rewrite_env_maps(*condition, current_env_map)),
            true_branch: Box::new(rewrite_env_maps(*true_branch, current_env_map)),
            false_branch: Box::new(rewrite_env_maps(*false_branch, current_env_map)),
        },
        Expr::Match { value, cases } => Expr::Match {
            value: Box::new(rewrite_env_maps(*value, current_env_map)),
            cases: cases
                .into_iter()
                .map(|(pattern, expr)| {
                    let var_count = var_count(&pattern);
                    let child_env_map: Vec<usize> = (0..var_count)
                        .chain(current_env_map.iter().map(|i| var_count + i))
                        .collect();
                    (
                        rewrite_env_map_pattern(pattern, current_env_map),
                        rewrite_env_maps(expr, &child_env_map),
                    )
                })
                .collect(),
        },
        Expr::NumericConstant(_) => expr,
        Expr::StringConstant(_) => expr,
        Expr::BooleanConstant(_) => expr,
        Expr::Unit => expr,
        Expr::Builtin(_) => expr,
        Expr::Loop { initial_vars, body } => {
            let new_initial_vars: Vec<Expr> = initial_vars
                .into_iter()
                .map(|e| rewrite_env_maps(e, current_env_map))
                .collect();
            let child_env_map: Vec<usize> = (0..new_initial_vars.len())
                .chain(current_env_map.iter().map(|i| new_initial_vars.len() + i))
                .collect();
            let new_body = rewrite_env_maps(*body, &child_env_map);
            Expr::Loop {
                initial_vars: new_initial_vars,
                body: Box::new(new_body),
            }
        }
        Expr::Next(arguments) => Expr::Next(
            arguments
                .into_iter()
                .map(|e| rewrite_env_maps(e, current_env_map))
                .collect(),
        ),
    }
}

fn strip_comments(
    sexpr: Located<Sexpr<Token<String>>>,
) -> Result<Located<Sexpr<Token<String>>>, Located<ParseError>> {
    match sexpr.value {
        Sexpr::Atom(Token::Comment(_)) => Err(sexpr.with_value(ParseError::InvalidSyntax)),
        Sexpr::Atom(_) => Ok(sexpr),
        Sexpr::List(mut list) => {
            if list.len() == 2 {
                match &list[0].value {
                    Sexpr::Atom(Token::Comment(_)) => return Ok(strip_comments(list.remove(1))?),
                    _ => {}
                }
            }
            let mut result = Vec::new();
            for e in list {
                result.push(strip_comments(e)?);
            }
            Ok(Located::new(sexpr.source_range, Sexpr::List(result)))
        }
    }
}

pub fn compile(s: &str) -> Result<Expr, Located<ParseError>> {
    let sexpr = parse(
        s,
        |s, quoted| {
            token::read_token(s, |s| {
                if quoted {
                    unescape_string(s).unwrap()
                } else {
                    s.to_string()
                }
            })
            .map_err(|e| ParseError::InvalidToken(e))
        },
        ParseError::UnexpectedCharacter,
    )?;
    let sexpr_no_comments = strip_comments(sexpr)?;
    let (expr, used_vars) = compile_sexpr(&sexpr_no_comments, &mut VecDeque::new())?;
    if !used_vars.is_empty() {
        panic!("Vars referenced that don't exist, should be impossible");
    }
    Ok(rewrite_env_maps(expr, &[]))
}
