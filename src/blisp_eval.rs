
use std::rc::Rc;

use crate::blisp_expr::{
    BLispExpr,
    BLispEnv,
    BLispEvalResult,
};

use crate::blisp_lexer::{
    BLispError,
    BLispErrorType,
};

pub fn evaluate(mut expr: BLispExpr, mut env: Rc<BLispEnv>) -> BLispEvalResult {
    loop {
        match expr {
            BLispExpr::SExp(first, rest) => {
                let first = evaluate(*first, env.clone());
                let rest = *rest;
                match (first, rest) {
                    (BLispEvalResult::Result(BLispExpr::Function(fn_ptr)), rest) => {
                        match evaluate_list_items(rest, env.clone()) {
                            BLispEvalResult::Result(rest) => 
                                match fn_ptr(rest, env) {
                                    BLispEvalResult::Result(expr) => return BLispEvalResult::Result(expr),
                                    BLispEvalResult::Error(error) => return BLispEvalResult::Error(error),
                                    BLispEvalResult::TailCall(next_expr, next_env) => { expr = next_expr; env = next_env; continue; }
                                },
                            BLispEvalResult::Error(error) => return BLispEvalResult::Error(error),
                            BLispEvalResult::TailCall(expr, _) =>
                                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("TailCall found as evaluation of function arguments: {}", expr), None)),
                        }
                    },
                    (BLispEvalResult::Result(BLispExpr::SpecialForm(fn_ptr)), rest) => {
                        match fn_ptr(rest, env) {
                            BLispEvalResult::Result(expr) => return BLispEvalResult::Result(expr),
                            BLispEvalResult::Error(error) => return BLispEvalResult::Error(error),
                            BLispEvalResult::TailCall(next_expr, next_env) => { expr = next_expr; env = next_env; continue; }
                        }
                    },
                    (BLispEvalResult::Result(BLispExpr::Lambda(arg_list, next_expr, local_env)), rest) => {
                        match evaluate_list_items(rest, env.clone()) {
                            BLispEvalResult::Result(rest) => {
                                expr = *next_expr;
                                env = match BLispEnv::bind(local_env.clone(), *arg_list.clone(), rest) {
                                    Ok(result) => Rc::new(result),
                                    Err(message) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("{} While binding lambda to: {}", message, arg_list), None)),
                                };
                                continue;
                            },
                            BLispEvalResult::Error(error) => return BLispEvalResult::Error(error),
                            BLispEvalResult::TailCall(expr, _) =>
                                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("TailCall found as evaluation of lambda arguments: {}", expr), None)),
                        }
                    },
                    (BLispEvalResult::Result(BLispExpr::Macro(arg_list, next_expr)), rest) => {
                        expr = *next_expr;
                        env = match BLispEnv::bind(env.clone(), *arg_list.clone(), rest) {
                            Ok(result) => Rc::new(result),
                            Err(message) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("{} While binding macro to: {}", message, arg_list), None)),
                        };
                        continue
                    },
                    (BLispEvalResult::Result(item), _) => {
                        return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Unapplicable first element in list: {}", item), None))
                    },
                    (BLispEvalResult::Error(error), _) => return BLispEvalResult::Error(error),
                    (result, _) => {
                        return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Unknown result in list: {}", result), None))
                    }
                }
            },
            BLispExpr::Symbol(string) =>
                match env.get(&string) {
                    Some(result) => return BLispEvalResult::Result(result.clone()),
                    None => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Unbound symbol evaluated: {}", string), None)),
                },
            expr => return BLispEvalResult::Result(expr)
        }
    }
}

fn evaluate_list_items(expr: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = expr {
        match (evaluate(*first, env.clone()), evaluate_list_items(*rest, env.clone())) {
            (BLispEvalResult::Result(eval_first), BLispEvalResult::Result(eval_rest)) =>
                return BLispEvalResult::Result(BLispExpr::cons_sexp(eval_first, eval_rest)),
            (BLispEvalResult::Error(err), _)
                | (_, BLispEvalResult::Error(err)) => return BLispEvalResult::Error(err),
            (BLispEvalResult::Result(_), result)
                | (result, _) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Unexpected result found as evaluation of list elements: {}", result), None)),
        }
    } else if expr == BLispExpr::Nil {
        return BLispEvalResult::Result(BLispExpr::Nil)
    }

    return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Misformed list found"), None))
}

#[cfg(test)]
mod blisp_eval_tests {
    use super::*;
    use crate::blisp_func::default_env;

    #[test]
    fn evaluates_simple_lists() {
        assert_eq!(
            evaluate(
                BLispExpr::cons_sexp(
                    BLispExpr::Symbol("cons".to_string()),
                    BLispExpr::cons_sexp(
                        BLispExpr::Number(5),
                        BLispExpr::cons_sexp(
                            BLispExpr::cons_sexp(
                                BLispExpr::Symbol("cons".to_string()),
                                BLispExpr::cons_sexp(
                                    BLispExpr::Number(6),
                                    BLispExpr::cons_sexp(
                                        BLispExpr::Nil,
                                        BLispExpr::Nil
                                    )
                                )
                            ),
                            BLispExpr::Nil
                        )
                    )
                ),
                Rc::new(default_env())
            ),
            BLispExpr::cons_sexp(
                BLispExpr::Number(5),
                BLispExpr::cons_sexp(
                    BLispExpr::Number(6),
                    BLispExpr::Nil
                )
            )
        );

        assert_eq!(
            evaluate(
                BLispExpr::cons_sexp(
                    BLispExpr::Symbol("+".to_string()),
                    BLispExpr::cons_sexp(
                        BLispExpr::Number(5),
                        BLispExpr::cons_sexp(
                            BLispExpr::Number(6),
                            BLispExpr::Nil
                        )
                    )
                ),
                Rc::new(default_env())
            ),
            BLispExpr::Number(11)
        );
    }
}
