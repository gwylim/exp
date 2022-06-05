use crate::builtin::Builtin;
use crate::expr::{Declaration, Expr};
use crate::types::{Free, Scheme, SchemeVar, Substitution, Type};
use crate::value::VecType;
use std::cell::RefCell;
use std::collections::vec_deque::VecDeque;
use std::collections::{HashMap, HashSet};
use std::iter::zip;

#[derive(Debug)]
pub enum TypeError {
    UnificationFailed(Type<u64>, Type<u64>),
    InfiniteType,
    InvalidNext,
}

fn instantiate_type(ty: &Type<SchemeVar<u64>>, var_offset: u64) -> Type<u64> {
    match ty {
        Type::Array(t) => Type::Array(Box::new(instantiate_type(t, var_offset))),
        Type::Function { argument, result } => Type::Function {
            argument: Box::new(instantiate_type(argument, var_offset)),
            result: Box::new(instantiate_type(result, var_offset)),
        },
        Type::Variable(SchemeVar::Bound(i)) => Type::Variable(var_offset + *i as u64),
        Type::Variable(SchemeVar::Free(x)) => Type::Variable(*x),
        Type::Number => Type::Number,
        Type::String => Type::String,
        Type::Boolean => Type::Boolean,
        Type::Unit => Type::Unit,
        Type::Tuple(value_types) => Type::Tuple(
            value_types
                .iter()
                .map(|t| instantiate_type(t, var_offset))
                .collect(),
        ),
    }
}

fn instantiate(scheme: &Scheme<u64>, next_var: &RefCell<u64>) -> Type<u64> {
    let var_offset = next_var.take();
    next_var.replace(var_offset + scheme.vars as u64);
    instantiate_type(&scheme.ty, var_offset)
}

fn get_var(next_var: &RefCell<u64>) -> u64 {
    let var = next_var.take();
    next_var.replace(var + 1);
    var
}

fn occurs_check(var: u64, ty: &Type<u64>) -> Result<(), TypeError> {
    match ty {
        Type::Array(t) => occurs_check(var, t),
        Type::Tuple(value_types) => {
            for t in value_types {
                occurs_check(var, t)?;
            }
            Ok(())
        }
        Type::Function { argument, result } => {
            occurs_check(var, argument)?;
            occurs_check(var, result)?;
            Ok(())
        }
        Type::Variable(x) => {
            if var == *x {
                return Err(TypeError::InfiniteType);
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn unify_variable(
    var: u64,
    ty: &Type<u64>,
    subst: &mut Substitution<u64>,
) -> Result<(), TypeError> {
    match subst.get(&var) {
        None => {
            if let Type::Variable(x) = ty {
                if let Some(ty1) = subst.get(x) {
                    return unify_variable(var, &ty1.clone(), subst);
                }
            }
            occurs_check(var, &most_general_unifier(ty, subst))?;
            subst.insert(var, ty.clone());
            Ok(())
        }
        Some(ty1) => unify(ty, &ty1.clone(), subst),
    }
}

fn most_general_unifier(ty: &Type<u64>, subst: &mut Substitution<u64>) -> Type<u64> {
    match ty {
        Type::Number => Type::Number,
        Type::String => Type::String,
        Type::Boolean => Type::Boolean,
        Type::Unit => Type::Unit,
        Type::Array(t) => Type::Array(Box::new(most_general_unifier(t, subst))),
        Type::Tuple(ts) => Type::Tuple(ts.iter().map(|t| most_general_unifier(t, subst)).collect()),
        Type::Function { argument, result } => Type::Function {
            argument: Box::new(most_general_unifier(argument, subst)),
            result: Box::new(most_general_unifier(result, subst)),
        },
        Type::Variable(v) => {
            match v.free() {
                None => Type::Variable(v.clone()),
                Some(x) => {
                    match subst.get(&x) {
                        None => Type::Variable(v.clone()),
                        Some(ty1) => {
                            let result = most_general_unifier(&ty1.clone(), subst);
                            // FIXME: don't clone here when not necessary
                            subst.insert(x, result.clone());
                            result
                        }
                    }
                }
            }
        }
    }
}

fn unify(
    type1: &Type<u64>,
    type2: &Type<u64>,
    subst: &mut Substitution<u64>,
) -> Result<(), TypeError> {
    if type1 == type2 {
        return Ok(());
    }
    if let Type::Variable(x) = type1 {
        return unify_variable(*x, type2, subst);
    }
    if let Type::Variable(x) = type2 {
        return unify_variable(*x, type1, subst);
    }
    match type1 {
        Type::Function { argument, result } => {
            if let Type::Function {
                argument: argument1,
                result: result1,
            } = type2
            {
                unify(argument, argument1, subst)?;
                unify(result, result1, subst)?;
                Ok(())
            } else {
                Err(TypeError::UnificationFailed(type1.clone(), type2.clone()))
            }
        }
        Type::Number => {
            if let Type::Number = type2 {
                Ok(())
            } else {
                Err(TypeError::UnificationFailed(type1.clone(), type2.clone()))
            }
        }
        Type::String => {
            if let Type::String = type2 {
                Ok(())
            } else {
                Err(TypeError::UnificationFailed(type1.clone(), type2.clone()))
            }
        }
        Type::Boolean => {
            if let Type::Boolean = type2 {
                Ok(())
            } else {
                Err(TypeError::UnificationFailed(type1.clone(), type2.clone()))
            }
        }
        Type::Unit => {
            if let Type::Unit = type2 {
                Ok(())
            } else {
                Err(TypeError::UnificationFailed(type1.clone(), type2.clone()))
            }
        }
        Type::Array(t) => {
            if let Type::Array(t1) = type2 {
                unify(t, t1, subst)
            } else {
                Err(TypeError::UnificationFailed(type1.clone(), type2.clone()))
            }
        }
        Type::Tuple(ts) => {
            if let Type::Tuple(ts1) = type2 {
                if ts.len() != ts1.len() {
                    return Err(TypeError::UnificationFailed(type1.clone(), type2.clone()));
                }
                for (t1, t2) in zip(ts, ts1) {
                    unify(t1, t2, subst)?;
                }
                Ok(())
            } else {
                Err(TypeError::UnificationFailed(type1.clone(), type2.clone()))
            }
        }
        Type::Variable(_) => unreachable!(),
    }
}

fn get_type_free_vars<V: Free<u64>>(
    ty: &Type<V>,
    subst: &mut Substitution<u64>,
    result: &mut HashSet<u64>,
) {
    match ty {
        Type::Array(t) => get_type_free_vars(t, subst, result),
        Type::Tuple(ts) => {
            for t in ts {
                get_type_free_vars(t, subst, result);
            }
        }
        Type::Function {
            argument,
            result: function_result,
        } => {
            get_type_free_vars(argument, subst, result);
            get_type_free_vars(function_result, subst, result);
        }
        Type::Variable(v) => {
            if let Some(x) = v.free() {
                let mgu = most_general_unifier(&Type::Variable(x), subst);
                if let Type::Variable(x) = mgu {
                    result.insert(x);
                } else {
                    get_type_free_vars(&mgu, subst, result);
                }
            }
        }
        Type::Number => {}
        Type::String => {}
        Type::Boolean => {}
        Type::Unit => {}
    }
}

fn free_vars(env: &VecDeque<Scheme<u64>>, subst: &mut Substitution<u64>) -> HashSet<u64> {
    let mut result = HashSet::new();
    for scheme in env {
        get_type_free_vars(&scheme.ty, subst, &mut result);
    }
    result
}

fn generalize(ty: Type<u64>, bound_vars_map: &HashMap<u64, usize>) -> Type<SchemeVar<u64>> {
    match ty {
        Type::Array(t) => Type::Array(Box::new(generalize(*t, bound_vars_map))),
        Type::Tuple(ts) => Type::Tuple(
            ts.into_iter()
                .map(|t| generalize(t, bound_vars_map))
                .collect(),
        ),
        Type::Function { argument, result } => Type::Function {
            argument: Box::new(generalize(*argument, bound_vars_map)),
            result: Box::new(generalize(*result, bound_vars_map)),
        },
        Type::Variable(x) => match bound_vars_map.get(&x) {
            None => Type::Variable(SchemeVar::Free(x)),
            Some(i) => Type::Variable(SchemeVar::Bound(*i)),
        },
        Type::Number => Type::Number,
        Type::String => Type::String,
        Type::Boolean => Type::Boolean,
        Type::Unit => Type::Boolean,
    }
}

fn function_type<V>(arguments: Vec<Type<V>>, result: Type<V>) -> Type<V> {
    let mut function = result;
    for argument in arguments.into_iter().rev() {
        function = Type::Function {
            argument: Box::new(argument),
            result: Box::new(function),
        };
    }
    function
}

fn builtin_type(builtin: Builtin, next_var: &RefCell<u64>) -> Type<u64> {
    match builtin {
        Builtin::Print => function_type(vec![Type::String], Type::Unit),
        Builtin::GetLine => function_type(vec![Type::Unit], Type::String),
        Builtin::Length => {
            let var = Type::Variable(get_var(next_var));
            function_type(vec![Type::Array(Box::new(var))], Type::Number)
        }
        Builtin::Get => {
            let var = Type::Variable(get_var(next_var));
            function_type(vec![Type::Array(Box::new(var.clone())), Type::Number], var)
        }
        Builtin::Concat => {
            let var = Type::Variable(get_var(next_var));
            let array = Type::Array(Box::new(var));
            function_type(vec![array.clone(), array.clone()], array)
        }
        Builtin::Add => function_type(vec![Type::Number, Type::Number], Type::Number),
        Builtin::Sub => function_type(vec![Type::Number, Type::Number], Type::Number),
        Builtin::Mul => function_type(vec![Type::Number, Type::Number], Type::Number),
        Builtin::Div => function_type(vec![Type::Number, Type::Number], Type::Number),
        Builtin::Mod => function_type(vec![Type::Number, Type::Number], Type::Number),
        Builtin::StringLength => function_type(vec![Type::String], Type::Number),
        Builtin::Substring => {
            function_type(vec![Type::String, Type::Number, Type::Number], Type::String)
        }
        Builtin::ConcatString => function_type(vec![Type::String, Type::String], Type::String),
        Builtin::Eq => {
            // TODO: this doesn't actually work for all types
            let var = Type::Variable(get_var(next_var));
            function_type(vec![var.clone(), var], Type::Boolean)
        }
        Builtin::NumberToString => function_type(vec![Type::Number], Type::String),
        Builtin::Leq => function_type(vec![Type::Number, Type::Number], Type::Boolean),
        Builtin::And => function_type(vec![Type::Boolean, Type::Boolean], Type::Boolean),
        Builtin::Or => function_type(vec![Type::Boolean, Type::Boolean], Type::Boolean),
        Builtin::Not => function_type(vec![Type::Boolean, Type::Boolean], Type::Boolean),
    }
}

fn typecheck_inner(
    expr: &Expr,
    env: &mut VecDeque<Scheme<u64>>,
    loop_types: Option<(&Type<u64>, &Vec<Type<u64>>)>,
    subst: &mut Substitution<u64>,
    next_var: &RefCell<u64>,
) -> Result<Type<u64>, TypeError> {
    let result = match expr {
        Expr::NumericConstant(_) => Ok(Type::Number),
        Expr::StringConstant(_) => Ok(Type::String),
        Expr::BooleanConstant(_) => Ok(Type::Boolean),
        Expr::Unit => Ok(Type::Unit),
        Expr::Function { arity, body, .. } => {
            let mut vars = Vec::new();
            for _ in 0..*arity {
                let var = get_var(next_var);
                vars.push(var);
                env.push_front(Scheme {
                    vars: 0,
                    ty: Type::Variable(SchemeVar::Free(var)),
                });
            }
            let result_type = typecheck_inner(&*body, env, None, subst, next_var)?;
            let mut function_type = result_type;
            for var in vars.into_iter() {
                env.pop_front();
                function_type = Type::Function {
                    argument: Box::new(Type::Variable(var)),
                    result: Box::new(function_type),
                };
            }
            Ok(function_type)
        }
        Expr::Apply {
            function,
            arguments,
        } => {
            let result_type = Type::Variable(get_var(next_var));
            let mut function_type = result_type.clone();
            for arg in arguments.iter().rev() {
                function_type = Type::Function {
                    argument: Box::new(typecheck_inner(arg, env, None, subst, next_var)?),
                    result: Box::new(function_type),
                };
            }
            unify(
                &typecheck_inner(function, env, None, subst, next_var)?,
                &function_type,
                subst,
            )?;
            Ok(result_type)
        }
        Expr::Bound(i) => Ok(instantiate(&env[*i], next_var)),
        Expr::Declarations { decls, body } => {
            for decl in decls {
                match decl {
                    Declaration::Let(expr) => {
                        let var = get_var(next_var);
                        env.push_front(Scheme {
                            ty: Type::Variable(SchemeVar::Free(var)),
                            vars: 0,
                        });
                        let ty = typecheck_inner(expr, env, None, subst, next_var)?;
                        env.pop_front();
                        unify_variable(var, &ty, subst)?;
                        let inferred_type = most_general_unifier(&ty, subst);
                        let env_free_vars = free_vars(env, subst);
                        let mut ty_free_vars = HashSet::new();
                        get_type_free_vars(&inferred_type, subst, &mut ty_free_vars);
                        let bound_vars = ty_free_vars
                            .into_iter()
                            .filter(|x| !env_free_vars.contains(x))
                            .zip(0..)
                            .collect();
                        env.push_front(Scheme {
                            ty: generalize(inferred_type, &bound_vars),
                            vars: bound_vars.len(),
                        });
                    }
                    Declaration::Type(_) => todo!(),
                }
            }
            let body_type = typecheck_inner(body, env, loop_types, subst, next_var)?;
            for _ in decls {
                env.pop_front();
            }
            Ok(body_type)
        }
        Expr::Builtin(b) => Ok(builtin_type(*b, next_var)),
        Expr::If {
            condition,
            true_branch,
            false_branch,
        } => {
            unify(
                &Type::Boolean,
                &typecheck_inner(condition, env, None, subst, next_var)?,
                subst,
            )?;
            let result_type = typecheck_inner(true_branch, env, loop_types, subst, next_var)?;
            unify(
                &result_type,
                &typecheck_inner(false_branch, env, loop_types, subst, next_var)?,
                subst,
            )?;
            Ok(result_type)
        }
        Expr::Match { .. } => todo!(),
        Expr::Loop { initial_vars, body } => {
            let mut var_types = Vec::new();
            for expr in initial_vars {
                let var = get_var(next_var);
                let var_type = typecheck_inner(expr, env, None, subst, next_var)?;
                unify_variable(var, &var_type, subst)?;
                var_types.push(var_type);
                env.push_front(Scheme {
                    vars: 0,
                    ty: Type::Variable(SchemeVar::Free(var)),
                });
            }
            let result_type = Type::Variable(get_var(next_var));
            let body_type =
                typecheck_inner(body, env, Some((&result_type, &var_types)), subst, next_var)?;
            for _ in initial_vars {
                env.pop_front();
            }
            unify(&result_type, &body_type, subst)?;
            Ok(body_type)
        }
        Expr::Next(args) => match loop_types {
            None => Err(TypeError::InvalidNext),
            Some((result_type, var_types)) => {
                if args.len() != var_types.len() {
                    return Err(TypeError::InvalidNext);
                }
                for (arg, t) in zip(args, var_types) {
                    unify(t, &typecheck_inner(arg, env, None, subst, next_var)?, subst)?;
                }
                Ok(result_type.clone())
            }
        },
        Expr::Vec {
            vec_type: VecType::Tuple,
            values,
        } => {
            let mut value_types = Vec::new();
            for value in values {
                value_types.push(typecheck_inner(value, env, None, subst, next_var)?);
            }
            Ok(Type::Tuple(value_types))
        }
        Expr::Vec {
            vec_type: VecType::Array,
            values,
        } => match values.split_first() {
            None => {
                let var = get_var(next_var);
                Ok(Type::Array(Box::new(Type::Variable(var))))
            }
            Some((v, vs)) => {
                let t = typecheck_inner(v, env, None, subst, next_var)?;
                for v in vs {
                    unify(&t, &typecheck_inner(v, env, None, subst, next_var)?, subst)?;
                }
                Ok(Type::Array(Box::new(t)))
            }
        },
    }?;
    Ok(result)
}

// TODO: check no free variables and convert to Type<!>
pub fn typecheck(expr: &Expr) -> Result<Type<u64>, TypeError> {
    let mut subst = HashMap::new();
    let ty = typecheck_inner(
        expr,
        &mut VecDeque::new(),
        None,
        &mut subst,
        &RefCell::new(0),
    )?;
    let result = most_general_unifier(&ty, &mut subst);
    let vars: Vec<u64> = subst.keys().map(|x| *x).collect();
    for var in vars {
        most_general_unifier(&Type::Variable(var), &mut subst);
    }
    Ok(result)
}
