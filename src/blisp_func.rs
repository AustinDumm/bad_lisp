
use crate::blisp_expr::{
    BLispExpr,
    BLispEnv,
};

//=============== List manipulation ===============
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


//=============== Numerical Operations ===============
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

fn sub(args: BLispExpr) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispExpr::Number(first - second),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispExpr::Float(first - second as f64),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispExpr::Float(first as f64 - second),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispExpr::Float(first - second),
                    (_, _) => panic!("Unexpected types to add"),
                }
            }
        }
    }

    panic!("Add expects two arguments");
}

fn mul(args: BLispExpr) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispExpr::Number(first * second),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispExpr::Float(first * second as f64),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispExpr::Float(first as f64 * second),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispExpr::Float(first * second),
                    (_, _) => panic!("Unexpected types to add"),
                }
            }
        }
    }

    panic!("Add expects two arguments");
}

fn int_div(args: BLispExpr) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispExpr::Number(first / second),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispExpr::Float(first / second as f64),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispExpr::Float(first as f64 / second),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispExpr::Float(first / second),
                    (_, _) => panic!("Unexpected types to add"),
                }
            }
        }
    }

    panic!("Add expects two arguments");
}

fn div(args: BLispExpr) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispExpr::Float(first as f64 / second as f64),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispExpr::Float(first / second as f64),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispExpr::Float(first as f64 / second),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispExpr::Float(first / second),
                    (_, _) => panic!("Unexpected types to add"),
                }
            }
        }
    }

    panic!("Add expects two arguments");
}
//=============== Default Environment ===============
pub fn default_env() -> BLispEnv {
    let mut env = BLispEnv::new();
    env.insert("+".to_string(), BLispExpr::Function(add));
    env.insert("-".to_string(), BLispExpr::Function(sub));
    env.insert("*".to_string(), BLispExpr::Function(mul));
    env.insert("/".to_string(), BLispExpr::Function(int_div));
    env.insert("//".to_string(), BLispExpr::Function(div));
    env.insert("cons".to_string(), BLispExpr::Function(cons));

    env
}

