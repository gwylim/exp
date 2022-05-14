use crate::builtin::invoke_builtin;
use crate::eq::eq;
use crate::program::{Expr, PatternExpr};
use crate::value::{RunResult, RuntimeError, Value};
use std::cell::RefCell;
use std::cmp::min;
use std::collections::vec_deque::VecDeque;
use std::rc::Rc;

fn apply<'a>(
    label: &RefCell<u64>,
    function_value: Rc<Value<'a>>,
    argument_values: VecDeque<Rc<Value<'a>>>,
) -> RunResult<'a> {
    match &*function_value {
        Value::Closure { arity, body, env } => {
            if *arity != argument_values.len() {
                return Err(RuntimeError::InvalidNumberOfArguments {
                    expected: *arity,
                    received: argument_values.len(),
                });
            }
            let mut stack = argument_values;
            stack.extend(env.iter().map(|v| match **v {
                Value::Recurse => Rc::new((*function_value).clone()),
                _ => v.clone(),
            }));
            run_inner(label, body, &mut stack)
        }
        Value::Builtin(b) => invoke_builtin(*b, argument_values),
        _ => Err(RuntimeError::AppliedNonFunction),
    }
}

fn create_closure<'a>(
    arity: usize,
    body: &'a Expr,
    parent_env: &VecDeque<Rc<Value<'a>>>,
    env_map: &[usize],
) -> Value<'a> {
    Value::Closure {
        arity,
        body,
        env: env_map.iter().map(|&i| parent_env[i].clone()).collect(),
    }
}

fn match_pattern<'a>(
    pattern: &'a PatternExpr,
    value: &Rc<Value<'a>>,
    stack: &VecDeque<Rc<Value<'a>>>,
    matches: &mut Vec<Rc<Value<'a>>>,
) -> Result<bool, RuntimeError> {
    match pattern {
        PatternExpr::BindVar => {
            matches.push(value.clone());
            Ok(true)
        }
        PatternExpr::Vec(pattern_vec) => {
            if let Value::Vec(vec) = &**value {
                for i in 0..min(pattern_vec.len(), vec.len()) {
                    if !match_pattern(&pattern_vec[i], &vec[i], stack, matches)? {
                        return Ok(false);
                    }
                }
                if pattern_vec.len() != vec.len() {
                    return Err(RuntimeError::TypeError);
                }
                Ok(true)
            } else {
                Err(RuntimeError::TypeError)
            }
        }
        e => {
            let matched = match e {
                PatternExpr::NumericConstant(n) => eq(&Value::Number(*n), &*value),
                PatternExpr::StringConstant(s) =>
                /* TODO: shouldn't clone string here */
                {
                    eq(&Value::String(s.clone()), &*value)
                }

                PatternExpr::BooleanConstant(b) => eq(&Value::Boolean(*b), &*value),
                PatternExpr::Unit => eq(&Value::Unit, &*value),
                PatternExpr::Bound(i) => eq(&stack[*i], &*value),
                PatternExpr::BindVar => unreachable!(),
                PatternExpr::Vec(_) => unreachable!(),
            }?;
            Ok(matched)
        }
    }
}

fn run_inner<'a>(
    label: &RefCell<u64>,
    expr: &'a Expr,
    stack: &mut VecDeque<Rc<Value<'a>>>,
) -> RunResult<'a> {
    match expr {
        &Expr::Function {
            arity,
            ref body,
            ref env_map,
        } => Ok(Rc::new(create_closure(arity, body, stack, env_map))),
        &Expr::Bound(i) => Ok(stack[i].clone()),
        Expr::Apply {
            function,
            arguments,
        } => {
            let function_value = run_inner(label, function, stack)?;
            let mut argument_values = VecDeque::new();
            for argument in arguments {
                argument_values.push_back(run_inner(label, argument, stack)?);
            }
            apply(label, function_value, argument_values)
        }
        &Expr::NumericConstant(x) => Ok(Rc::new(Value::Number(x))),
        Expr::StringConstant(s) => Ok(Rc::new(Value::String(s.to_string()))),
        Expr::BooleanConstant(b) => Ok(Rc::new(Value::Boolean(*b))),
        Expr::Builtin(b) => Ok(Rc::new(Value::Builtin(*b))),
        Expr::Unit => Ok(Rc::new(Value::Unit)),
        Expr::Let { value, body } => {
            stack.push_front(Rc::new(Value::Recurse));
            stack[0] = run_inner(label, &value, stack)?;
            let result = run_inner(label, &body, stack);
            stack.pop_front();
            result
        }
        Expr::Vec(vec) => {
            let mut result = Vec::new();
            for e in vec {
                result.push(run_inner(label, e, stack)?);
            }
            Ok(Rc::new(Value::Vec(result)))
        }
        Expr::If {
            condition,
            true_branch,
            false_branch,
        } => {
            let condition_result = run_inner(label, condition, stack)?;
            match &*condition_result {
                Value::Boolean(b) => {
                    if *b {
                        run_inner(label, true_branch, stack)
                    } else {
                        run_inner(label, false_branch, stack)
                    }
                }
                _ => Err(RuntimeError::TypeError),
            }
        }
        Expr::Match { value, cases } => {
            let evaluated_value = run_inner(label, value, stack)?;
            let mut matched_case = None;
            for (pattern, body) in cases {
                let mut matches = Vec::new();
                let matched = match_pattern(pattern, &evaluated_value, stack, &mut matches)?;
                if matched {
                    if matched_case.is_some() {
                        return Err(RuntimeError::MultiplePatternsMatched);
                    }
                    matched_case = Some((body, matches));
                }
            }
            if matched_case.is_none() {
                return Err(RuntimeError::NoPatternsMatched);
            }
            let (body, matches) = matched_case.unwrap();
            let var_count = matches.len();
            for v in matches {
                stack.push_front(v);
            }
            let result = run_inner(label, body, stack);
            for _ in 0..var_count {
                stack.pop_front();
            }
            result
        }
        Expr::Label => {
            let next_label = label.take();
            label.replace(next_label + 1);
            Ok(Rc::new(Value::Label(next_label)))
        }
    }
}

pub fn run(expr: &Expr) -> RunResult {
    run_inner(&mut RefCell::new(0), expr, &mut VecDeque::new())
}

#[cfg(test)]
mod test {
    use crate::sexpr::sexpr_file;
    use crate::value::Value;
    use crate::{compile, run};

    // We pass a callback since the Expr doesn't live past this function and isn't owned by the
    // Value
    fn eval<F>(s: &str, f: F)
    where
        F: Fn(&Value),
    {
        f(&*run(&compile(&sexpr_file(s).unwrap().1).unwrap()).unwrap())
    }

    #[test]
    fn variable_binding() {
        eval(&"let $x 1 (let $y 2 x)", |val| {
            assert_eq!(val, &Value::Number(1.0))
        })
    }

    #[test]
    fn function_definition() {
        eval(&"let $f (fn $x; add x x); f 1", |val| {
            assert_eq!(val, &Value::Number(2.0))
        })
    }

    #[test]
    fn if_statement() {
        eval(&"if (eq 0 1) 1; if (eq 0 0) 2 3", |val| {
            assert_eq!(val, &Value::Number(2.0))
        })
    }

    #[test]
    fn tuple_matching() {
        eval(&"match #(1 \"[a string]) (1 $s) s (2 0) 0", |val| {
            assert_eq!(val, &Value::String("a string".to_string()))
        })
    }

    #[test]
    #[should_panic]
    fn match_type_error() {
        eval(
            &"match #(1 \"[a string]) (1 $s) s (\"[a string] 1) 1",
            |_val| {},
        )
    }

    #[test]
    fn recursion() {
        eval(
            &"let $fac (fn $n; if (eq n 0) 1; mul n; fac; sub n 1); fac 7",
            |val| assert_eq!(val, &Value::Number(5040.0)),
        )
    }

    #[test]
    fn closure() {
        eval(
            &"let $create_closure (fn $x; fn $y; add x y); let $f (create_closure 2); f 3",
            |val| assert_eq!(val, &Value::Number(5.0)),
        )
    }

    #[test]
    fn labels() {
        eval(&"let $l1 label; let $l2 label; match l1 l1 1 l2 2", |val| {
            assert_eq!(val, &Value::Number(1.0))
        })
    }
}
