use crate::expr::{Declaration, Expr, PatternExpr};
use crate::ParseError;

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
        Expr::Function { arity, body } => {
            check_no_recursion(body, level + arity)?;
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
        Expr::BytesConstant(_) => {}
        Expr::Unit => {}
        Expr::Builtin(_) => {}
    }
    Ok(())
}

// Checks that no references to bound variable at index 0 exist which aren't guarded by a function
pub fn check_recursion_guarded(expr: &Expr, level: usize) -> Result<(), ParseError> {
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
        Expr::BytesConstant(_) => {}
        Expr::Unit => {}
        Expr::Function { .. } => {}
        Expr::Builtin(_) => {}
    }
    Ok(())
}
