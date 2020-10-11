
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

pub fn default_env() -> BLispEnv {
    let mut env = BLispEnv::new();
    env.insert("+".to_string(), BLispExpr::Function(add));
    env.insert("cons".to_string(), BLispExpr::Function(cons));

    env
}
