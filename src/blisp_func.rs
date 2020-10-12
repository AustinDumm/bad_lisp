
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

fn list(args: BLispExpr) -> BLispExpr {
    match args {
        BLispExpr::SExp(_, _) => return args,
        _ => panic!("Misformed argument list given to list"),
    }
}


fn first(args: BLispExpr) -> BLispExpr {
    if let BLispExpr::SExp(arg, _) = args {
        if let BLispExpr::SExp(first, _) = *arg {
            return *first
        }
    }
    panic!("first must take list argument");
}

fn rest(args: BLispExpr) -> BLispExpr {
    if let BLispExpr::SExp(arg, _) = args {
        if let BLispExpr::SExp(_, rest) = *arg {
            return *rest
        }
    }
    panic!("rest must take list argument");
}

//=============== Numerical Operations ===============
fn incr(args: BLispExpr) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(num), BLispExpr::Nil) => return BLispExpr::Number(num + 1),
            (BLispExpr::Float(float), BLispExpr::Nil) => return BLispExpr::Float(float + 1.0),
            (_, _) => panic!("incr only takes a single numerical argument")
        }
    }
    panic!("Malformed argument list to incr")
}

fn decr(args: BLispExpr) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(num), BLispExpr::Nil) => return BLispExpr::Number(num - 1),
            (BLispExpr::Float(float), BLispExpr::Nil) => return BLispExpr::Float(float - 1.0),
            (_, _) => panic!("incr only takes a single numerical argument")
        }
    }
    panic!("Malformed argument list to incr")
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

    env.insert("incr".to_string(), BLispExpr::Function(incr));
    env.insert("decr".to_string(), BLispExpr::Function(decr));

    env.insert("+".to_string(), BLispExpr::Function(add));
    env.insert("-".to_string(), BLispExpr::Function(sub));
    env.insert("*".to_string(), BLispExpr::Function(mul));
    env.insert("/".to_string(), BLispExpr::Function(int_div));
    env.insert("//".to_string(), BLispExpr::Function(div));

    env.insert("first".to_string(), BLispExpr::Function(first));
    env.insert("rest".to_string(), BLispExpr::Function(rest));
    env.insert("list".to_string(), BLispExpr::Function(list));
    env.insert("cons".to_string(), BLispExpr::Function(cons));

    env
}

pub fn minimal_env() -> BLispEnv {
    let mut env = BLispEnv::new();
    
    env.insert("incr".to_string(), BLispExpr::Function(incr));
    env.insert("decr".to_string(), BLispExpr::Function(decr));

    env.insert("cons".to_string(), BLispExpr::Function(cons));
    env.insert("first".to_string(), BLispExpr::Function(first));
    env.insert("rest".to_string(), BLispExpr::Function(rest));

    env
}

