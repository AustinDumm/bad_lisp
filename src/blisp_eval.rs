
use num_traits::Num;

use crate::blisp_expr::{
    BLispExpr,
    BLispEnv,
};

fn cons(args: BLispExpr) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return BLispExpr::SExp(first, second)
            }
        }
    }

    panic!("cons must take exactly two arguments");
}

fn add(args: BLispExpr) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispExpr::Number(first + second),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispExpr::Float(first + second as f64),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispExpr::Float(first as f64 + second),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispExpr::Float(first + second),
                    (_, _) => panic!("Unexpected types to add"),
                }
            }
        }
    }

    panic!("Add expects two arguments");
}

fn default_env() -> BLispEnv {
    let mut env = BLispEnv::new();
    env.insert("+".to_string(), BLispExpr::Function(add));
    env.insert("cons".to_string(), BLispExpr::Function(cons));

    env
}

fn evaluate(expr: BLispExpr, env: &BLispEnv) -> BLispExpr {
    match expr {
        BLispExpr::SExp(_, _) => {
            if let BLispExpr::SExp(first, rest) = evaluate_list_items(expr, env) {
                match *first {
                    BLispExpr::Function(fn_ptr) => return fn_ptr(*rest),
                    expr => panic!("Unhandled list application: {}", expr),
                }
            }

            panic!("Unapplicable first element in list");
        },
        BLispExpr::Symbol(string) => env.get(&string).expect(&format!("Unbound symbol evaluated: {}", string)).clone(),
        expr => expr
    }
}

fn evaluate_list_items(expr: BLispExpr, env: &BLispEnv) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = expr {
        let eval_first = evaluate(*first, env);
        let eval_rest = evaluate_list_items(*rest, env);

        return BLispExpr::cons_sexp(eval_first, eval_rest);
    } else if expr == BLispExpr::Nil {
        return BLispExpr::Nil
    }

    panic!("Misformed list found");
}

#[cfg(test)]
mod blisp_eval_tests {
    use super::*;

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
                &default_env()
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
                &default_env()
            ),
            BLispExpr::Number(11)
        );
    }
}
