use crate::builtin;
use crate::builtin::invoke_builtin;
use crate::expr::Expr;
use crate::located::Located;
use crate::value::{Binding, EvalResult, Function, RuntimeError, SyntaxError, Value};
use hex::FromHex;
use std::collections::vec_deque::VecDeque;
use std::rc::Rc;

fn apply<'a>(
    function_value: Rc<Value<'a>>,
    argument_values: VecDeque<Rc<Value<'a>>>,
) -> EvalResult<'a> {
    match &*function_value {
        Value::Apply {
            function,
            arity,
            arguments,
        } => {
            let argument_count = arguments.len() + argument_values.len();
            if *arity > argument_count {
                // Partial application
                return Ok(Rc::new(Value::Apply {
                    arity: *arity,
                    function: function.clone(),
                    arguments: arguments
                        .clone()
                        .into_iter()
                        .chain(argument_values)
                        .collect(),
                }));
            }
            let mut arguments_to_function: VecDeque<(&'a str, Rc<Value<'a>>)> =
                arguments.clone().into_iter().collect();
            let mut remaining_arguments: VecDeque<Rc<Value<'a>>> = VecDeque::new();
            for arg in argument_values {
                if *arity > arguments_to_function.len() {
                    arguments_to_function.push_back(arg);
                } else {
                    remaining_arguments.push_back(arg);
                }
            }
            let result = match function {
                Function::Closure { body, env } => {
                    arguments_to_function.extend(env.iter().map(|v| match *v {
                        (s, &Value::Recurse) => (s, Rc::new((*function_value).clone())),
                        _ => v.clone(),
                    }));
                    eval_inner(body, &mut arguments_to_function)
                }
                Function::Builtin(b) => invoke_builtin(*b, arguments_to_function),
            }?;
            if remaining_arguments.is_empty() {
                Ok(result)
            } else {
                apply(result, remaining_arguments)
            }
        }
        _ => Err(RuntimeError::TypeError),
    }
}

fn create_closure<'a>(
    arity: usize,
    body: &'a Expr,
    parent_env: &VecDeque<Binding<'a>>,
) -> Value<'a> {
    Value::Apply {
        arity,
        arguments: Vec::new(),
        function: Function::Closure {
            body,
            env: parent_env.iter().map(|x| x.clone()).collect(),
        },
    }
}

fn eval_inner<'a>(expr: &'a Located<Expr>, stack: &mut VecDeque<Binding<'a>>) -> EvalResult<'a> {
    match expr.value {
        Expr::Atom(s) => {
            let first: char = s.chars().nth(0).unwrap();
            if first == '0' && s.chars().nth(1) == Some('x') {
                let result = <Vec<u8>>::from_hex(&s[2..]);
                return match result {
                    Ok(x) => Ok(Rc::new(Value::Bytes(x))),
                    Err(_) => Err(expr
                        .with_value(RuntimeError::SyntaxError(SyntaxError::InvalidBytesLiteral))),
                };
            }
            if first.is_numeric() || first == '-' {
                let result = s.parse::<i64>();
                match result {
                    Ok(x) => Ok(Rc::new(Value::Number(x))),
                    Err(_) => Err(expr.with_value(RuntimeError::SyntaxError(
                        SyntaxError::InvalidNumericLiteral,
                    ))),
                }
            } else {
                todo!()
            }
        }
        Expr::Cons(a, b) => match *a.value {
            Expr::Atom(s) => {
                todo!()
            }
            _ => Err(),
        },
    }
    /*
    match expr {
        &Expr::Function { arity, ref body } => Ok(Rc::new(create_closure(arity, body, stack))),
        &Expr::Bound(i) => Ok(stack[i].clone()),
        Expr::Apply {
            function,
            arguments,
        } => {
            let function_value = run_inner(function, stack)?;
            let mut argument_values = VecDeque::new();
            for argument in arguments {
                argument_values.push_back(run_inner(argument, stack)?);
            }
            apply(function_value, argument_values)
        }
        &Expr::NumericConstant(x) => Ok(Rc::new(Value::Number(x))),
        Expr::StringConstant(s) => Ok(Rc::new(Value::String(s.to_string()))),
        Expr::BooleanConstant(b) => Ok(Rc::new(Value::Boolean(*b))),
        Expr::BytesConstant(b) => Ok(Rc::new(Value::Bytes(b.clone()))),
        Expr::Builtin(b) => Ok(Rc::new(Value::Apply {
            arity: builtin::arity(*b),
            arguments: Vec::new(),
            function: Function::Builtin(*b),
        })),
        Expr::Unit => Ok(Rc::new(Value::Unit)),
        Expr::Declarations { decls, body } => {
            let mut var_count = 0;
            for decl in decls {
                match decl {
                    Declaration::Let(value) => {
                        stack.push_front(Rc::new(Value::Recurse));
                        stack[0] = run_inner(&value, stack)?;
                        var_count += 1;
                    }
                }
            }
            let result = run_inner(&body, stack);
            for _ in 0..var_count {
                stack.pop_front();
            }
            result
        }
        Expr::If {
            condition,
            true_branch,
            false_branch,
        } => {
            let condition_result = run_inner(condition, stack)?;
            match &*condition_result {
                Value::Boolean(b) => {
                    if *b {
                        run_inner(true_branch, stack)
                    } else {
                        run_inner(false_branch, stack)
                    }
                }
                _ => Err(RuntimeError::TypeError),
            }
        }
        Expr::Loop { initial_vars, body } => {
            let mut vars = Vec::new();
            for var in initial_vars {
                vars.push(run_inner(var, stack)?);
            }
            for var in vars {
                stack.push_front(var);
            }
            loop {
                let result = run_inner(body, stack)?;
                for _ in 0..initial_vars.len() {
                    stack.pop_front();
                }
                match &*result {
                    Value::Next(new_vars) => {
                        for var in new_vars {
                            stack.push_front(var.clone());
                        }
                    }
                    _ => break Ok(result),
                }
            }
        }
        Expr::Next(arguments) => {
            let mut args = Vec::new();
            for arg in arguments {
                args.push(run_inner(arg, stack)?);
            }
            Ok(Rc::new(Value::Next(args)))
        }
    }
     */
}

pub fn eval<'a>(expr: &'a Expr) -> EvalResult<'a> {
    eval_inner(expr, &mut VecDeque::new())
}

#[cfg(test)]
mod test {
    use crate::eval;
    use crate::expr::Expr;
    use crate::value::{RuntimeError, Value};

    // We pass a callback since the Expr doesn't live past this function and isn't owned by the
    // Value
    fn eval<F>(s: &str, f: F)
    where
        F: Fn(&Value),
    {
        f(&*eval(&Expr::parse(s).unwrap()).unwrap())
    }

    fn eval_error(s: &str) -> RuntimeError {
        eval(&compile(s).unwrap()).unwrap_err()
    }

    #[test]
    fn constant() {
        eval(&"123", |val| assert_eq!(val, &Value::Number(123.0)))
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
            eval_error(&"match (# 1 [\"a string]) (case (# 1 $s) s) (case (# 2 0) 0)"),
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
            &"data ($a _ _) ($b _ _); let $x (a 1 2); match x (case (a $y $z) z) (case (b _ _) 3)",
            |val| assert_eq!(val, &Value::Number(2.0)),
        )
    }

    #[test]
    fn match_different_type_constructor() {
        assert_eq!(
            eval_error(
                &"data ($a _); let $x (a 1); data ($b _); match x (case (a $x) x) (case (b $x) x)"
            ),
            RuntimeError::TypeError
        );
    }

    #[test]
    fn recursive_construction_and_match() {
        eval(
            &"data ($cons _ _) $nil\n\
                 let $list\n  \
                   fn $vec\n  \
                   let $build\n    \
                     fn $result $i\n    \
                     if (eq i 0) result\n    \
                     build (cons (get vec; sub i 1) result) (sub i 1)\n  \
                   build nil (length vec)\n\
                 let $sum\n  \
                   fn $list\n  \
                   match list\n    \
                     case nil 0\n  \
                   , case (cons $x $xs) (add x; sum xs)\n\
                 sum (list (@ 1 2 3 4 5))
        ",
            |val| assert_eq!(val, &Value::Number(15.0)),
        )
    }

    #[test]
    fn comparison_type_error() {
        assert_eq!(
            eval_error(&"data ($a _ _); if (eq (a 1 2) (a 2 [\"string])) true false"),
            RuntimeError::TypeError
        );
    }

    #[test]
    fn constructor_incorrect_argument_count() {
        assert_eq!(
            eval_error(&"data ($a _ _); a 0 1 2"),
            RuntimeError::AppliedNonFunction
        )
    }

    #[test]
    fn no_patterns_matched() {
        assert_eq!(
            eval_error(&"data ($a _) ($b _); let $x (a 1); match x (case (b $x) x)"),
            RuntimeError::NoPatternsMatched
        );
    }

    #[test]
    fn multiple_patterns_matched() {
        assert_eq!(
            eval_error(&"data ($a _) ($b _); let $x (a 1); match x (case $x x) (case (a $x) x)"),
            RuntimeError::MultiplePatternsMatched
        );
    }

    #[test]
    fn invalid_match_pattern() {
        assert_eq!(
            eval_error(&"data ($a _ _); match (a 1 1) (case (a $x) x)"),
            RuntimeError::InvalidConstructorPattern
        );
    }

    #[test]
    fn looping() {
        // Sum of first n triangular numbers: (n-2)(n-1)n / 6 = 8 * 9 * 10 / 6 = 120
        eval(
            &"loop (var $i 0) (var $j 0) (var $r 0)\n\
                 if (leq 10 j) r\n\
                 if (leq j i) (next 0 (add j 1) r)\n\
                 next (add i 1) j (add r i)",
            |val| assert_eq!(val, &Value::Number(120.0)),
        )
    }

    #[test]
    fn partial_application() {
        eval(
            &"let $f (fn $x $y $z; add x (add y z)); let $g (f 1); let $h (g 2); h 3",
            |val| assert_eq!(val, &Value::Number(6.0)),
        )
    }

    #[test]
    fn constructor_partial_application() {
        eval(
            &"data ($tuple _ _); let $x (tuple 1); let $y (x 2); let $z (x 3); eq y z",
            |val| assert_eq!(val, &Value::Boolean(false)),
        )
    }

    #[test]
    fn builtin_partial_application() {
        eval(&"let $add1 (add 1); add1 2", |val| {
            assert_eq!(val, &Value::Number(3.0))
        })
    }
}
