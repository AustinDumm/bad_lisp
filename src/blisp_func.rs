
use std::rc::Rc;

use crate::blisp_expr::{
    BLispExpr,
    BLispEnv,
};

use crate::blisp_eval::evaluate;

//=============== List manipulation ===============
fn cons(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return BLispExpr::SExp(first, second)
            }
        }
    }

    panic!("cons must take exactly two arguments");
}

fn list(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispExpr {
    match args {
        BLispExpr::SExp(_, _) => return args,
        _ => panic!("Misformed argument list given to list"),
    }
}


fn first(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispExpr {
    if let BLispExpr::SExp(arg, _) = args {
        if let BLispExpr::SExp(first, _) = *arg {
            return *first
        }
    }
    panic!("first must take list argument");
}

fn rest(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispExpr {
    if let BLispExpr::SExp(arg, _) = args {
        if let BLispExpr::SExp(_, rest) = *arg {
            return *rest
        }
    }
    panic!("rest must take list argument");
}

//=============== Numerical Operations ===============
fn incr(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(num), BLispExpr::Nil) => return BLispExpr::Number(num + 1),
            (BLispExpr::Float(float), BLispExpr::Nil) => return BLispExpr::Float(float + 1.0),
            (_, _) => panic!("incr only takes a single numerical argument")
        }
    }
    panic!("Malformed argument list to incr")
}

fn decr(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(num), BLispExpr::Nil) => return BLispExpr::Number(num - 1),
            (BLispExpr::Float(float), BLispExpr::Nil) => return BLispExpr::Float(float - 1.0),
            (_, _) => panic!("incr only takes a single numerical argument")
        }
    }
    panic!("Malformed argument list to incr")
}

fn add(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispExpr {
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

fn sub(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispExpr {
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

fn mul(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispExpr {
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

fn int_div(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispExpr {
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

fn div(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispExpr {
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

//=============== Comparison Predicates ===============
fn apply_predicate(args: BLispExpr, func: fn(BLispExpr, BLispExpr) -> BLispExpr) -> BLispExpr {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return func(*first, *second)
            }
        }
    }
    panic!("Predicates must take 2 arguments")
}

fn eq(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispExpr {
    apply_predicate(args, |first, second| {
        BLispExpr::Bool(first == second)
    })
}

//=============== Special Forms ===============
fn if_impl(args: BLispExpr, env: Rc<BLispEnv>) -> BLispExpr {
    if let BLispExpr::SExp(predicate, rest) = args {
        if let BLispExpr::SExp(first, rest) = *rest {
            if let BLispExpr::SExp(second, rest) = *rest {
                if *rest == BLispExpr::Nil {
                    match evaluate(*predicate, env.clone()) {
                        BLispExpr::Bool(true) => return evaluate(*first, env.clone()),
                        BLispExpr::Bool(false) => return evaluate(*second, env.clone()),
                        _ => panic!("First argument to \"if\" must evaluate to boolean")
                    }
                }
            }
        }
    }
    
    panic!("if requires predicate and two more arguments")
}

fn bind_single_binding(binding: BLispExpr, env: &mut BLispEnv) {
    if let BLispExpr::SExp(symbol, rest) = binding {
        if let BLispExpr::SExp(value, rest) = *rest {
            match (*symbol, *value, *rest) {
                (BLispExpr::Symbol(name), value, BLispExpr::Nil) => { env.insert(name, value); return }
                (_, _, _) => panic!("Binding must be two element list with first element being a symbol"),
            }
        }
    }
}

fn bind_from_binding_list(mut binding_list: BLispExpr, env: &mut BLispEnv) {
    while binding_list != BLispExpr::Nil {
        if let BLispExpr::SExp(binding, rest) = binding_list {
            bind_single_binding(*binding, env);
            binding_list = *rest;
        }
    }
}

fn let_impl(args: BLispExpr, env: Rc<BLispEnv>) -> BLispExpr {
    if let BLispExpr::SExp(binding_list, rest) = args {
        if let BLispExpr::SExp(expr, rest) = *rest {
            if *rest == BLispExpr::Nil {
                let mut child_env = BLispEnv::extend(env);
                bind_from_binding_list(*binding_list, &mut child_env);
                return evaluate(*expr, Rc::new(child_env))
            }
        }
    }

    panic!("let requires list of bindings and single expr to execute with bindings")
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

    env.insert("=".to_string(), BLispExpr::Function(eq));

    env.insert("if".to_string(), BLispExpr::SpecialForm(if_impl));
    env.insert("let".to_string(), BLispExpr::SpecialForm(let_impl));

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

