
use std::rc::Rc;

use crate::blisp_expr::{
    BLispExpr,
    BLispEnv,
};

pub fn evaluate(expr: BLispExpr, env: Rc<BLispEnv>) -> BLispExpr {
    match expr {
        BLispExpr::SExp(first, rest) => {
            let first = evaluate(*first, env.clone());
            let rest = *rest;
            match (first, rest) {
                (BLispExpr::Function(fn_ptr), rest) => {
                    let rest = evaluate_list_items(rest, env.clone());
                    return fn_ptr(rest, env);
                },
                (BLispExpr::SpecialForm(fn_ptr), rest) => {
                    return fn_ptr(rest, env);
                },
                (BLispExpr::Lambda(arg_list, expr, env), rest) => {
                    let rest = evaluate_list_items(rest, env.clone());
                    return evaluate(*expr, Rc::new(BLispEnv::bind(env, *arg_list, rest)));
                },
                (_, _) => {
                    panic!("Unapplicable first element in list");
                },
            }
        },
        BLispExpr::Symbol(string) => env.get(&string).expect(&format!("Unbound symbol evaluated: {}", string)).clone(),
        expr => expr
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
