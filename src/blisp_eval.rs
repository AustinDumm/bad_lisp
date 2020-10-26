
use std::rc::Rc;

use crate::blisp_expr::{
    BLispExpr,
    BLispEnv,
    BLispEvalResult,
};

pub fn evaluate(mut expr: BLispExpr, mut env: Rc<BLispEnv>) -> BLispExpr {
    loop {
        match expr {
            BLispExpr::SExp(first, rest) => {
                let first = evaluate(*first, env.clone());
                let rest = *rest;
                match (first, rest) {
                    (BLispExpr::Function(fn_ptr), rest) => {
                        let rest = evaluate_list_items(rest, env.clone());
                        match fn_ptr(rest, env) {
                            BLispEvalResult::Result(expr) => return expr,
                            BLispEvalResult::TailCall(next_expr, next_env) => { expr = next_expr; env = next_env; continue; }
                        }
                    },
                    (BLispExpr::SpecialForm(fn_ptr), rest) => {
                        match fn_ptr(rest, env) {
                            BLispEvalResult::Result(expr) => return expr,
                            BLispEvalResult::TailCall(next_expr, next_env) => { expr = next_expr; env = next_env; continue; }
                        }
                    },
                    (BLispExpr::Lambda(arg_list, next_expr, local_env), rest) => {
                        let rest = evaluate_list_items(rest, env.clone());
                        expr = *next_expr;
                        env = Rc::new(BLispEnv::bind(local_env.clone(), *arg_list, rest));
                        continue;
                    },
                    (BLispExpr::Macro(arg_list, next_expr), rest) => {
                        expr = *next_expr;
                        env = Rc::new(BLispEnv::bind(env.clone(), *arg_list, rest));
                        continue
                    },
                    (item, _) => {
                        panic!("Unapplicable first element in list: {}", item);
                    },
                }
            },
            BLispExpr::Symbol(string) => return env.get(&string).expect(&format!("Unbound symbol evaluated: {}", string)).clone(),
            expr => return expr
        }
    }
}

fn evaluate_list_items(expr: BLispExpr, env: Rc<BLispEnv>) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = expr {
        let eval_first = evaluate(*first, env.clone());
        let eval_rest = evaluate_list_items(*rest, env.clone());

        return BLispExpr::cons_sexp(eval_first, eval_rest);
    } else if expr == BLispExpr::Nil {
        return BLispExpr::Nil
    }

    panic!("Misformed list found");
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
