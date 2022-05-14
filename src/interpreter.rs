use crate::builtin::invoke_builtin;
use crate::eq::eq;
use crate::program::{Expr, PatternExpr};
use crate::value::{RunResult, RuntimeError, Value};
use std::cell::RefCell;
use std::collections::vec_deque::VecDeque;
use std::iter::zip;
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
        Value::Constructor {
            type_id,
            index,
            arity,
        } => {
            if *arity != argument_values.len() {
                return Err(RuntimeError::InvalidNumberOfArguments {
                    expected: *arity,
                    received: argument_values.len(),
                });
            }
            Ok(Rc::new(Value::Constructed {
                type_id: *type_id,
                index: *index,
                values: argument_values.into_iter().collect(),
            }))
        }
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
                if pattern_vec.len() != vec.len() {
                    return Err(RuntimeError::TypeError);
                }
                let mut matched = true;
                // We loop over all elements to catch any type errors
                for (x, y) in zip(pattern_vec, vec) {
                    if !match_pattern(x, y, stack, matches)? {
                        matched = false;
                    }
                }
                Ok(matched)
            } else {
                Err(RuntimeError::TypeError)
            }
        }
        PatternExpr::Constructed {
            constructor,
            arguments,
        } => {
            let constructor_value = &stack[*constructor];
            if let Value::Constructor {
                type_id,
                index,
                arity,
            } = &**constructor_value
            {
                if *arity != arguments.len() {
                    return Err(RuntimeError::InvalidConstructorPattern);
                }
                if let Value::Constructed {
                    type_id: type_id1,
                    index: index1,
                    values,
                } = &**value
                {
                    if type_id != type_id1 {
                        return Err(RuntimeError::TypeError);
                    }
                    if index != index1 {
                        return Ok(false);
                    }
                    let mut matched = true;
                    for (pattern, value) in zip(arguments, values) {
                        if !match_pattern(pattern, value, stack, matches)? {
                            matched = false;
                        }
                    }
                    Ok(matched)
                } else {
                    Err(RuntimeError::TypeError)
                }
            } else {
                Err(RuntimeError::InvalidConstructorPattern)
            }
        }
        e => {
            let matched = match e {
                PatternExpr::NumericConstant(n) => eq(&Value::Number(*n), &*value),
                PatternExpr::StringConstant(s) => {
                    // We don't call eq here to avoid having to clone the string
                    if let Value::String(s1) = &**value {
                        Ok(s == s1)
                    } else {
                        Err(RuntimeError::TypeError)
                    }
                }

                PatternExpr::BooleanConstant(b) => eq(&Value::Boolean(*b), &*value),
                PatternExpr::Unit => eq(&Value::Unit, &*value),
                PatternExpr::Bound(i) => eq(&stack[*i], &*value),
                PatternExpr::BindVar => unreachable!(),
                PatternExpr::Vec(_) => unreachable!(),
                PatternExpr::Constructed { .. } => unreachable!(),
            }?;
            Ok(matched)
        }
    }
}

fn create_id(unique_id: &RefCell<u64>) -> u64 {
    let next_id = unique_id.take();
    unique_id.replace(next_id + 1);
    next_id
}

fn run_inner<'a>(
    unique_id: &RefCell<u64>,
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
            let function_value = run_inner(unique_id, function, stack)?;
            let mut argument_values = VecDeque::new();
            for argument in arguments {
                argument_values.push_back(run_inner(unique_id, argument, stack)?);
            }
            apply(unique_id, function_value, argument_values)
        }
        &Expr::NumericConstant(x) => Ok(Rc::new(Value::Number(x))),
        Expr::StringConstant(s) => Ok(Rc::new(Value::String(s.to_string()))),
        Expr::BooleanConstant(b) => Ok(Rc::new(Value::Boolean(*b))),
        Expr::Builtin(b) => Ok(Rc::new(Value::Builtin(*b))),
        Expr::Unit => Ok(Rc::new(Value::Unit)),
        Expr::Let { value, body } => {
            stack.push_front(Rc::new(Value::Recurse));
            stack[0] = run_inner(unique_id, &value, stack)?;
            let result = run_inner(unique_id, &body, stack);
            stack.pop_front();
            result
        }
        Expr::Vec(vec) => {
            let mut result = Vec::new();
            for e in vec {
                result.push(run_inner(unique_id, e, stack)?);
            }
            Ok(Rc::new(Value::Vec(result)))
        }
        Expr::If {
            condition,
            true_branch,
            false_branch,
        } => {
            let condition_result = run_inner(unique_id, condition, stack)?;
            match &*condition_result {
                Value::Boolean(b) => {
                    if *b {
                        run_inner(unique_id, true_branch, stack)
                    } else {
                        run_inner(unique_id, false_branch, stack)
                    }
                }
                _ => Err(RuntimeError::TypeError),
            }
        }
        Expr::Match { value, cases } => {
            let evaluated_value = run_inner(unique_id, value, stack)?;
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
            let result = run_inner(unique_id, body, stack);
            for _ in 0..var_count {
                stack.pop_front();
            }
            result
        }
        Expr::TypeDeclaration { constructors, body } => {
            // TODO: It might make more sense to assign this id when compiling rather than here
            let type_id = create_id(unique_id);
            for (index, arity) in constructors.iter().enumerate() {
                stack.push_front(Rc::new(if *arity > 0 {
                    Value::Constructor {
                        type_id,
                        index,
                        arity: *arity,
                    }
                } else {
                    // arity-0 constructors are not invoked
                    Value::Constructed {
                        type_id,
                        index,
                        values: Vec::new(),
                    }
                }));
            }
            let result = run_inner(unique_id, body, stack);
            for _ in 0..constructors.len() {
                stack.pop_front();
            }
            result
        }
    }
}

pub fn run(expr: &Expr) -> RunResult {
    run_inner(&mut RefCell::new(0), expr, &mut VecDeque::new())
}

#[cfg(test)]
mod test {
    use crate::sexpr::sexpr_file;
    use crate::value::{RuntimeError, Value};
    use crate::{compile, run};

    // We pass a callback since the Expr doesn't live past this function and isn't owned by the
    // Value
    fn eval<F>(s: &str, f: F)
    where
        F: Fn(&Value),
    {
        f(&*run(&compile(&sexpr_file(s).unwrap().1).unwrap()).unwrap())
    }

    fn eval_error(s: &str) -> RuntimeError {
        run(&compile(&sexpr_file(s).unwrap().1).unwrap()).unwrap_err()
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
    fn applied_non_function() {
        assert_eq!(
            eval_error(&"let $x 1; x 1"),
            RuntimeError::AppliedNonFunction
        );
    }

    #[test]
    fn if_statement() {
        eval(&"if (eq 0 1) 1; if (eq 0 0) 2 3", |val| {
            assert_eq!(val, &Value::Number(2.0))
        })
    }

    #[test]
    fn match_type_error() {
        assert_eq!(
            eval_error(&"match #(1 \"[a string]) #(1 $s) s #(2 0) 0"),
            RuntimeError::TypeError
        );
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
    fn match_constructor() {
        eval(
            &"data ($a _ _) ($b _ _); let $x (a 1 2); match x (a $y $z) z (b $_ $_) 3",
            |val| assert_eq!(val, &Value::Number(2.0)),
        )
    }

    #[test]
    fn match_different_type_constructor() {
        assert_eq!(
            eval_error(&"data ($a _); let $x (a 1); data ($b _); match x (a $x) x (b $x) x"),
            RuntimeError::TypeError
        );
    }

    #[test]
    fn recursive_construction_and_match() {
        eval(
            &"
            data ($cons _ _) $nil
          ; let $list (
              fn $vec
            ; let $build (
                fn $result $i
              ; if (eq i 0) result
              ; build (cons (get vec; sub i 1) result) (sub i 1)
              )
            ; build nil (length vec)
            )
          ; let $sum (
              fn $list
            ; match list
                (cons $x $xs) (add x; sum xs)
                nil 0
            )
          ; sum (list #(1 2 3 4 5))
        ",
            |val| assert_eq!(val, &Value::Number(15.0)),
        )
    }

    #[test]
    fn comparison_type_error() {
        assert_eq!(
            eval_error(&"data ($a _ _); if (eq (a 1 2) (a 2 \"[string])) true false",),
            RuntimeError::TypeError
        );
    }

    #[test]
    fn constructor_incorrect_argument_count() {
        assert_eq!(
            eval_error(&"data ($a _ _); a 0"),
            RuntimeError::InvalidNumberOfArguments {
                expected: 2,
                received: 1
            }
        )
    }

    #[test]
    fn no_patterns_matched() {
        assert_eq!(
            eval_error(&"data ($a _) ($b _); let $x (a 1); match x (b $x) x"),
            RuntimeError::NoPatternsMatched
        );
    }

    #[test]
    fn multiple_patterns_matched() {
        assert_eq!(
            eval_error(&"data ($a _) ($b _); let $x (a 1); match x $x x (a $x) x"),
            RuntimeError::MultiplePatternsMatched
        );
    }

    #[test]
    fn invalid_match_pattern() {
        assert_eq!(
            eval_error(&"data ($a _ _); match (a 1 1) (a $x) x"),
            RuntimeError::InvalidConstructorPattern
        );
    }
}
