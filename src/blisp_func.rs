
use std::io;
use std::fs;

use std::rc::Rc;
use std::convert::TryInto;

use crate::blisp_expr::{
    BLispExpr,
    BLispEnv,
    BLispEvalResult,
};

use crate::blisp_lexer;

use crate::blisp_eval::evaluate;
use crate::blisp_parser::{
    self,
    parse_string_literal,
};

//=============== Utilities ===============
fn apply_predicate<F>(args: BLispExpr, func: F) -> BLispEvalResult 
where F: Fn(BLispExpr, BLispExpr) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return func(*first, *second)
            }
        }
    }
    panic!("Predicates must take 2 arguments")
}

fn exit(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        if *rest == BLispExpr::Nil {
            match *first {
                BLispExpr::Number(value) => std::process::exit(value.try_into().expect("Exit code given too large to fit into 32 bits")),
                _ => panic!("Invalid code given to exit"),
            }
        }
    } else if args == BLispExpr::Nil {
        std::process::exit(0);
    }

    panic!("Malformed arguments list given to exit");
}

fn seq(mut args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    while let BLispExpr::SExp(first, second) = args {
        if *second == BLispExpr::Nil {
            return BLispEvalResult::Result(*first);
        } else {
            args = *second;
        }
    }
    
    panic!("Malformed list given to sequence");
}

fn collect_string(mut string_expr: BLispExpr) -> String {
    let mut output = "".to_string();
    while let BLispExpr::SExp(first, rest) = string_expr {
        match (*first, *rest) {
            (BLispExpr::Char(character), rest) => { output += &String::from(character); string_expr = rest; },
            (first, rest) => panic!("Expected list of characters ({} . {})", first, rest),
        }
    }

    output
}

//=============== List manipulation ===============
fn cons(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return BLispEvalResult::Result(BLispExpr::SExp(first, second))
            }
        }
    }

    panic!("cons must take exactly two arguments");
}

fn list(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    match args {
        BLispExpr::SExp(_, _) => return BLispEvalResult::Result(args),
        _ => panic!("Misformed argument list given to list"),
    }
}


fn first(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(arg, _) = args {
        if let BLispExpr::SExp(first, _) = *arg {
            return BLispEvalResult::Result(*first)
        }
        panic!("\"first\" passed argument that is not a list: {}", arg);
    }
    panic!("first must take list argument");
}

fn rest(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(arg, _) = args {
        if let BLispExpr::SExp(_, rest) = *arg {
            return BLispEvalResult::Result(*rest)
        }
        panic!("rest given argument: {}", arg);
    }
    panic!("rest must take list argument");
}

//=============== Numerical Operations ===============
fn incr(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(num), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Number(num + 1)),
            (BLispExpr::Float(float), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Float(float + 1.0)),
            (_, _) => panic!("incr only takes a single numerical argument")
        }
    }
    panic!("Malformed argument list to incr")
}

fn decr(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(num), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Number(num - 1)),
            (BLispExpr::Float(float), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Float(float - 1.0)),
            (_, _) => panic!("incr only takes a single numerical argument")
        }
    }
    panic!("Malformed argument list to incr")
}

fn abs(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(num), BLispExpr::Nil) => BLispEvalResult::Result(BLispExpr::Number(num.abs())),
            (BLispExpr::Float(float), BLispExpr::Nil) => BLispEvalResult::Result(BLispExpr::Float(float.abs())),
            (_, _) => panic!("abs only takes a single numberical argument"),
        }
    } else {
        panic!("Malformed argument list to abs")
    }
}

fn sign(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(num), BLispExpr::Nil) => BLispEvalResult::Result(BLispExpr::Number(num.signum())),
            (BLispExpr::Float(float), BLispExpr::Nil) => BLispEvalResult::Result(BLispExpr::Float(float.signum())),
            (_, _) => panic!("abs only takes a single numberical argument"),
        }
    } else {
        panic!("Malformed argument list to abs")
    }
}

fn add(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispEvalResult::Result(BLispExpr::Number(first + second)),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Float(first + second as f64)),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first as f64 + second)),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first + second)),
                    (_, _) => panic!("Unexpected types to add"),
                }
            }
        }
    }

    panic!("Add expects two arguments");
}

fn sub(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispEvalResult::Result(BLispExpr::Number(first - second)),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Float(first - second as f64)),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first as f64 - second)),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first - second)),
                    (_, _) => panic!("Unexpected types to sub"),
                }
            }
        }
    }

    panic!("Add expects two arguments");
}

fn mul(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispEvalResult::Result(BLispExpr::Number(first * second)),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Float(first * second as f64)),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first as f64 * second)),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first * second)),
                    (first, second) => panic!("Unexpected types to mul (* {} {})", first, second),
                }
            }
        }
    }

    panic!("Add expects two arguments");
}

fn int_div(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispEvalResult::Result(BLispExpr::Number(first / second)),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Float(first / second as f64)),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first as f64 / second)),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first / second)),
                    (_, _) => panic!("Unexpected types to int div"),
                }
            }
        }
    }

    panic!("Add expects two arguments");
}

fn div(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispEvalResult::Result(BLispExpr::Float(first as f64 / second as f64)),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Float(first / second as f64)),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first as f64 / second)),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first / second)),
                    (_, _) => panic!("Unexpected types to div"),
                }
            }
        }
    }

    panic!("Add expects two arguments");
}

fn modulo(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispEvalResult::Result(BLispExpr::Number(first % second)),
                    (_, _) => panic!("Unexpected types to modulo"),
                }
            }
        }
    }

    panic!("Add expects two arguments");
}

//=============== Logical Operators ===============
fn not(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Bool(value), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(!value)),
            (first, rest) => panic!("not must take single, boolean argument ({} . {})", first, rest),
        }
    }
    panic!("Malformed argument give to not");
}

fn and(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match evaluate(first, env.clone()) {
            BLispEvalResult::Result(BLispExpr::Bool(true)) => {
                match evaluate(second, env.clone()) {
                    BLispEvalResult::Result(BLispExpr::Bool(value)) => return BLispEvalResult::TailCall(BLispExpr::Bool(value), env.clone()),
                    _ => panic!("Second argument to \"and\" not boolean"),
                }
            },
            BLispEvalResult::Result(BLispExpr::Bool(false)) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            _ => panic!("First argument to \"and\" not boolean")
        }
    })
}

fn or(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match evaluate(first, env.clone()) {
            BLispEvalResult::Result(BLispExpr::Bool(false)) => {
                match evaluate(second, env.clone()) {
                    BLispEvalResult::Result(BLispExpr::Bool(value)) => return BLispEvalResult::TailCall(BLispExpr::Bool(value), env.clone()),
                    _ => panic!("Second argument to \"or\" not boolean"),
                }
            },
            BLispEvalResult::Result(BLispExpr::Bool(true)) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            _ => panic!("First argument to \"and\" not boolean")
        }
    })
}

fn xor(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Bool(first), BLispExpr::Bool(second)) => return BLispEvalResult::Result(BLispExpr::Bool((first || second) && !(first && second))),
            (_, _) => panic!("\"and\" predicate must be given to boolean arguments"),
        }
    })
}

//=============== Bitwise Operators ===============
fn b_not(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(value), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Number(!value)),
            (_, _) => panic!("Bitwise not must recieve single Number as argument")
        }
    }

    panic!("Malformed list given to bitwise not");
}

fn b_and(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => return BLispEvalResult::Result(BLispExpr::Number(first & second)),
            (_, _) => panic!("Bitwise and must take two Number arguements"),
        }
    })
}

fn b_or(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => return BLispEvalResult::Result(BLispExpr::Number(first | second)),
            (_, _) => panic!("Bitwise and must take two Number arguements"),
        }
    })
}

fn b_xor(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => return BLispEvalResult::Result(BLispExpr::Number(first ^ second)),
            (_, _) => panic!("Bitwise and must take two Number arguements"),
        }
    })
}

//=============== Type Checks ===============
fn is_nil(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Nil, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (_, _) => panic!("Too many arguments provided to Nil check"),
        }
    }
    panic!("Malformed list given to Nil check")
}

fn is_bool(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Bool(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (_, _) => panic!("Too many arguments provided to Bool check"),
        }
    }
    panic!("Malformed list given to Bool check")
}

fn is_number(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (_, _) => panic!("Too many arguments provided to Number check"),
        }
    }
    panic!("Malformed list given to Number check")
}

fn is_float(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Float(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (_, _) => panic!("Too many arguments provided to Float check"),
        }
    }
    panic!("Malformed list given to Float check")
}

fn is_char(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Char(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (_, _) => panic!("Too many arguments provided to Char check"),
        }
    }
    panic!("Malformed list given to Char check")
}

fn is_symbol(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Symbol(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (_, _) => panic!("Too many arguments provided to Symbol check"),
        }
    }
    panic!("Malformed list given to Symbol check")
}

fn is_applicable(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::SpecialForm(_), BLispExpr::Nil) |
            (BLispExpr::Function(_), BLispExpr::Nil) |
            (BLispExpr::Lambda(_, _, _), BLispExpr::Nil) |
            (BLispExpr::Macro(_, _), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (_, _) => panic!("Too many arguments provided to Applicable check"),
        }
    }
    panic!("Malformed list given to Applicable check")
}

fn is_special_form(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::SpecialForm(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (_, _) => panic!("Too many arguments provided to Special Form check"),
        }
    }
    panic!("Malformed list given to Special Form check")
}

fn is_function(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Function(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (_, _) => panic!("Too many arguments provided to Function check"),
        }
    }
    panic!("Malformed list given to Function check")
}

fn is_list(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::SExp(_, _), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (_, _) => panic!("Too many arguments provided to List check"),
        }
    }
    panic!("Malformed list given to List check")
}
//=============== Comparison Predicates ===============
fn eq(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        BLispEvalResult::Result(BLispExpr::Bool(first == second))
    })
}

fn neq(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        BLispEvalResult::Result(BLispExpr::Bool(first != second))
    })
}

fn lt(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first < second)),
            (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first < second as f64)),
            (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool((first as f64) < second)),
            (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool(first < second)),
            (_, _) => panic!("Incompatible types given to less than"),
        }
    })
}

fn lte(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first <= second)),
            (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first <= second as f64)),
            (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool((first as f64) <= second)),
            (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool(first <= second)),
            (_, _) => panic!("Incompatible types given to less than"),
        }
    })
}

fn gt(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first > second)),
            (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first > second as f64)),
            (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool((first as f64) > second)),
            (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool(first > second)),
            (_, _) => panic!("Incompatible types given to less than"),
        }
    })
}

fn gte(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first >= second)),
            (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first >= second as f64)),
            (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool((first as f64) >= second)),
            (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool(first >= second)),
            (_, _) => panic!("Incompatible types given to less than"),
        }
    })
}

//=============== Special Forms ===============
fn if_impl(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(predicate, rest) = args {
        if let BLispExpr::SExp(first, rest) = *rest {
            if let BLispExpr::SExp(second, rest) = *rest {
                if *rest == BLispExpr::Nil {
                    match evaluate(*predicate, env.clone()) {
                        BLispEvalResult::Result(BLispExpr::Bool(true)) => return BLispEvalResult::TailCall(*first, env),
                        BLispEvalResult::Result(BLispExpr::Bool(false)) => return BLispEvalResult::TailCall(*second, env),
                        result => panic!("First argument to \"if\" must evaluate to boolean: {:?} found", result)
                    }
                }
            }
        }
    }
    
    panic!("if requires predicate and two more arguments")
}

fn bind_single_binding(binding: BLispExpr, eval_env: Rc<BLispEnv>, env: &mut BLispEnv) {
    if let BLispExpr::SExp(symbol, rest) = binding {
        if let BLispExpr::SExp(value, rest) = *rest {
            match (*symbol, *value, *rest) {
                (BLispExpr::Symbol(name), value, BLispExpr::Nil) => { 
                    match evaluate(value, eval_env) {
                        BLispEvalResult::Result(value) => env.insert(name, value),
                        BLispEvalResult::Error(error) => panic!("Error found evaluating let binding"),
                        BLispEvalResult::TailCall(_, _) => panic!("TailCall found as result to binding list expr"),
                    }
                }
                (_, _, _) => panic!("Binding must be two element list with first element being a symbol"),
            }
        }
    }
}

fn bind_from_binding_list(mut binding_list: BLispExpr, eval_env: Rc<BLispEnv>, env: &mut BLispEnv) {
    while binding_list != BLispExpr::Nil {
        if let BLispExpr::SExp(binding, rest) = binding_list {
            bind_single_binding(*binding, eval_env.clone(), env);
            binding_list = *rest;
        }
    }
}

fn bind_from_seq_binding_list(mut binding_list: BLispExpr, mut env: BLispEnv) -> BLispEnv {
    while binding_list != BLispExpr::Nil {
        if let BLispExpr::SExp(binding, rest) = binding_list {
            let last_env = Rc::new(env);
            env = BLispEnv::extend(last_env.clone());
            bind_single_binding(*binding, last_env, &mut env);
            binding_list = *rest;
        }
    }
    env
}

fn let_impl(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(binding_list, rest) = args.clone() {
        if let BLispExpr::SExp(expr, rest) = *rest {
            if *rest == BLispExpr::Nil {
                let mut child_env = BLispEnv::extend(env.clone());
                bind_from_binding_list(*binding_list, env.clone(), &mut child_env);
                return BLispEvalResult::TailCall(*expr, Rc::new(child_env))
            }
        }
    }

    panic!("let requires list of bindings and single expr to execute with bindings. \nFound: {}", args)
}

fn let_seq(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(binding_list, rest) = args {
        if let BLispExpr::SExp(expr, rest) = *rest {
            if *rest == BLispExpr::Nil {
                let child_env = BLispEnv::extend(env.clone());
                let child_env = bind_from_seq_binding_list(*binding_list, child_env);
                return BLispEvalResult::TailCall(*expr, Rc::new(child_env))
            }
        }
    }

    panic!("let* requires list of bindings and single expr to execute with bindings")
}

fn dyn_let(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
     if let BLispExpr::SExp(binding_expr, rest) = args {
        if let BLispExpr::SExp(expr, rest) = *rest {
            if *rest == BLispExpr::Nil {
                match evaluate(*binding_expr, env.clone()) {
                    BLispEvalResult::Result(binding_list) => {
                        let child_env = BLispEnv::extend(env.clone());
                        let child_env = bind_from_seq_binding_list(binding_list, child_env);
                        return BLispEvalResult::TailCall(*expr, Rc::new(child_env))
                    },
                    BLispEvalResult::Error(error) => return BLispEvalResult::Error(error),
                    BLispEvalResult::TailCall(_, _) => panic!("TailCall found as result of dyn-let binding"),
                }
            }
        }
    }

    panic!("dyn-let requires list of bindings and single expr to execute with bindings")
}

fn load(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(file_name, rest) = args {
        match (*file_name, *rest) {
            (file_name, BLispExpr::Nil) => {
                let loaded = fs::read_to_string(collect_string(file_name)).expect("Failed to open file").parse().expect("Failed to read file");
                match blisp_lexer::lex(loaded) {
                    Ok(mut list) => 
                        match blisp_parser::parse(&mut list) {
                            Ok(ast) => return BLispEvalResult::TailCall(ast, env.clone()),
                            Err(error) => panic!("{}", error),
                        }
                    Err(error) => panic!("{}", error),
                }
            },
            (_, _) => panic!("Load must take single filename argument"),
        }
    }
    panic!("Malformed list passed to load");
}

//=============== Lambda Creation ===============
fn lambda(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(binding_list, rest) = args {
        if let BLispExpr::SExp(eval_expr, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return BLispEvalResult::Result(BLispExpr::Lambda(binding_list, eval_expr, env));
            }
        }
    }

    panic!("Malformed arguments given to lambda")
}

//=============== Input/Output ===============
fn print_std(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        if *rest == BLispExpr::Nil {
            println!("{}", collect_string(*first));
            return BLispEvalResult::Result(BLispExpr::Bool(true));
        }
    }
    panic!("Malformed list given to print")
}

fn debug(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        if *rest == BLispExpr::Nil {
            println!("{}", first);
            return BLispEvalResult::Result(BLispExpr::Bool(true));
        }
    }
    panic!("Malformed argument given to debug")
}

fn read_std(_args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).expect("Failed to read from standard in");
    buffer.pop();
    buffer = format!("\"{}\"", buffer);
    match blisp_lexer::lex(buffer.chars().collect()) {
        Ok(mut list) => 
            match parse_string_literal(&mut list) {
                Ok(parsed) => BLispEvalResult::TailCall(parsed, env.clone()),
                Err(error) => panic!("{}", error),
            }
        Err(error) => panic!("{}", error),
    }
}

//=============== Macros ===============
pub fn quote(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (first, BLispExpr::Nil) => BLispEvalResult::Result(first),
            (_, _) => panic!("quote must take single argument"),
        }
    } else {
        panic!("Malformed list given to quote")
    }
}

pub fn quasiquote(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
    fn quasi_list_item(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
        if let BLispExpr::SExp(first, arg_list) = args.clone() {
            if *first == BLispExpr::Symbol(String::from("unquote")) {
                if let BLispExpr::SExp(arg, nil) = *arg_list {
                    if *nil == BLispExpr::Nil {
                        return evaluate(*arg, env.clone());
                    }
                }
                panic!("unquote must take single argument");
            } else {
                return quasi_list(args, env)
            }
        }

        BLispEvalResult::Result(args)
    }

    fn quasi_list(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
        if let BLispExpr::SExp(first, rest) = args.clone() {
            if *first == BLispExpr::Symbol(String::from("unquote")) {
                quasi_list_item(args, env)
            } else {
                match (quasi_list_item(*first, env.clone()), quasi_list(*rest, env.clone())) {
                    (BLispEvalResult::Result(item), BLispEvalResult::Result(rest)) => BLispEvalResult::Result(BLispExpr::cons_sexp(item, rest)),
                    (BLispEvalResult::Error(error), _) |
                        (_, BLispEvalResult::Error(error)) => return BLispEvalResult::Error(error),
                    (BLispEvalResult::TailCall(_, _), _) |
                        (_, BLispEvalResult::TailCall(_, _)) => panic!("TailCall returned as result of quasiquoted list"),
                }
            }
        } else {
            quasi_list_item(args, env.clone())
        }
    }

    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (first, BLispExpr::Nil) => {
                match quasi_list(first, env) {
                    BLispEvalResult::Result(item) => return BLispEvalResult::Result(item),
                    BLispEvalResult::Error(error) => return BLispEvalResult::Error(error),
                    BLispEvalResult::TailCall(_, _) => panic!("TailCall returned as a result of quasiquoted list"),
                }
            },
            (_, _) => panic!("Quasiquote can only take single argument")
        }
    }
    panic!("Malformed list given to quasiquote")
}

fn def_macro(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(binding_list, rest) = args {
        if let BLispExpr::SExp(eval_expr, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return BLispEvalResult::Result(BLispExpr::Macro(binding_list, eval_expr));
            }
        }
    }

    panic!("Malformed arguments given to macro")
}

fn eval(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (first, BLispExpr::Nil) => BLispEvalResult::TailCall(first, env),
            (_, _) => panic!("eval must take single argument"),
        }
    } else {
        panic!("Malformed list given to quote")
    }
}

fn exec(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (first, BLispExpr::Nil) => {
                let string = collect_string(first);
                match blisp_lexer::lex(string) {
                    Ok(mut list) => 
                        match blisp_parser::parse(&mut list) {
                            Ok(ast) => return BLispEvalResult::TailCall(ast, env.clone()),
                            Err(error) => panic!("{}", error),
                        }
                    Err(error) => panic!("{}", error),
                }
            },
            (_, _) => panic!("Single argument must be passed to exec")
        }
    }
    panic!("Malformed string given to exec");
}

fn parse(args: BLispExpr, _env: Rc<BLispEnv>) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (first, BLispExpr::Nil) => {
                let string = collect_string(first);
                match blisp_lexer::lex(string) {
                    Ok(mut list) => 
                        match blisp_parser::parse(&mut list) {
                            Ok(ast) => return BLispEvalResult::Result(ast),
                            Err(error) => panic!("{}", error),
                        }
                    Err(error) => panic!("{}", error),
                }
            },
            (_, _) => panic!("Single argument must be passed to exec")
        }
    }
    panic!("Malformed string given to exec");
}

//=============== Default Environment ===============
pub fn default_env() -> BLispEnv {
    let mut env = BLispEnv::new();

    env.insert("incr".to_string(), BLispExpr::Function(incr));
    env.insert("decr".to_string(), BLispExpr::Function(decr));
    env.insert("abs".to_string(), BLispExpr::Function(abs));
    env.insert("sign".to_string(), BLispExpr::Function(sign));

    env.insert("+".to_string(), BLispExpr::Function(add));
    env.insert("-".to_string(), BLispExpr::Function(sub));
    env.insert("*".to_string(), BLispExpr::Function(mul));
    env.insert("/".to_string(), BLispExpr::Function(int_div));
    env.insert("//".to_string(), BLispExpr::Function(div));
    env.insert("mod".to_string(), BLispExpr::Function(modulo));

    env.insert("bit_not".to_string(), BLispExpr::Function(b_not));
    env.insert("bit_and".to_string(), BLispExpr::Function(b_and));
    env.insert("bit_or".to_string(), BLispExpr::Function(b_or));
    env.insert("bit_xor".to_string(), BLispExpr::Function(b_xor));

    env.insert("cons".to_string(), BLispExpr::Function(cons));
    env.insert("first".to_string(), BLispExpr::Function(first));
    env.insert("rest".to_string(), BLispExpr::Function(rest));
    env.insert("list".to_string(), BLispExpr::Function(list));

    env.insert("not".to_string(), BLispExpr::Function(not));
    env.insert("and".to_string(), BLispExpr::SpecialForm(and));
    env.insert("or".to_string(), BLispExpr::SpecialForm(or));
    env.insert("xor".to_string(), BLispExpr::Function(xor));

    env.insert("=".to_string(), BLispExpr::Function(eq));
    env.insert("!=".to_string(), BLispExpr::Function(neq));
    env.insert("<".to_string(), BLispExpr::Function(lt));
    env.insert("<=".to_string(), BLispExpr::Function(lte));
    env.insert(">".to_string(), BLispExpr::Function(gt));
    env.insert(">=".to_string(), BLispExpr::Function(gte));

    env.insert("nil?".to_string(), BLispExpr::Function(is_nil));
    env.insert("bool?".to_string(), BLispExpr::Function(is_bool));
    env.insert("number?".to_string(), BLispExpr::Function(is_number));
    env.insert("float?".to_string(), BLispExpr::Function(is_float));
    env.insert("char?".to_string(), BLispExpr::Function(is_char));
    env.insert("symbol?".to_string(), BLispExpr::Function(is_symbol));
    env.insert("applicable?".to_string(), BLispExpr::Function(is_applicable));
    env.insert("special_form?".to_string(), BLispExpr::Function(is_special_form));
    env.insert("function?".to_string(), BLispExpr::Function(is_function));
    env.insert("list?".to_string(), BLispExpr::Function(is_list));

    env.insert("if".to_string(), BLispExpr::SpecialForm(if_impl));
    env.insert("let".to_string(), BLispExpr::SpecialForm(let_impl));
    env.insert("let*".to_string(), BLispExpr::SpecialForm(let_seq));
    env.insert("dyn-let".to_string(), BLispExpr::SpecialForm(dyn_let));
    env.insert("seq".to_string(), BLispExpr::Function(seq));
    env.insert("load".to_string(), BLispExpr::Function(load));

    env.insert("lambda".to_string(), BLispExpr::SpecialForm(lambda));
    env.insert("macro".to_string(), BLispExpr::SpecialForm(def_macro));
    env.insert("eval".to_string(), BLispExpr::Function(eval));
    env.insert("quote".to_string(), BLispExpr::SpecialForm(quote));
    env.insert("quasiquote".to_string(), BLispExpr::SpecialForm(quasiquote));
    env.insert("unquote".to_string(), BLispExpr::Function(eval));
    env.insert("exec".to_string(), BLispExpr::Function(exec));
    env.insert("parse".to_string(), BLispExpr::Function(parse));

    env.insert("exit".to_string(), BLispExpr::Function(exit));

    env.insert("print".to_string(), BLispExpr::Function(print_std));
    env.insert("debug".to_string(), BLispExpr::Function(debug));
    env.insert("read".to_string(), BLispExpr::Function(read_std));

    env
}

