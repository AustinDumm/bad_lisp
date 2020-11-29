
use std::io;
use std::fs;

use std::rc::Rc;
use std::convert::TryInto;

use crate::types::{
    BLispExpr,
    BLispEvalResult,
    BLispEnv,
    BLispError,
    BLispErrorType,
    BLispCallStack,
    BLispFrame,
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
    if let BLispExpr::SExp(first, rest) = args.clone() {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return func(*first, *second)
            }
        }
    }
    return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Predicates must take 2 arguments. Found: {}", args), None))
}

fn exit(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        if *rest == BLispExpr::Nil {
            match *first {
                BLispExpr::Number(value) => std::process::exit(value.try_into().expect("Exit code given too large to fit into 32 bits")),
                _ => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Invalid code given to exit. Found: {}", *first), None)),
            }
        }
        return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("exit expects single argument. Found: {}", args), None))
    } else if args == BLispExpr::Nil {
        std::process::exit(0);
    }

    return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed arguments list given to exit. Found: {}", args), None))
}

fn seq(mut args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    while let BLispExpr::SExp(first, second) = args {
        if *second == BLispExpr::Nil {
            return BLispEvalResult::Result(*first);
        } else {
            args = *second;
        }
    }
    
    return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to sequence. Found: {}", args), None))
}

fn collect_string(mut string_expr: BLispExpr) -> String {
    let mut output = "".to_string();
    while let BLispExpr::SExp(first, rest) = string_expr {
        match (*first, *rest) {
            (BLispExpr::Char(character), rest) => { output += &String::from(character); string_expr = rest; },
            (first, rest) => panic!("Expected list of characters ({} . {})", first, rest)
        }
    }

    output
}

//=============== List manipulation ===============
fn cons(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return BLispEvalResult::Result(BLispExpr::SExp(first, second))
            }
        }
    }

    return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("cons must take exactly two arguments. Found: {}", args), None))
}

fn list(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    match args {
        BLispExpr::SExp(_, _) => return BLispEvalResult::Result(args),
        _ => BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Misformed argument list given to list. {}", args), None))
    }
}


fn first(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(arg, _) = args {
        if let BLispExpr::SExp(first, _) = *arg {
            return BLispEvalResult::Result(*first)
        }
        return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("\"first\" passed argument that is not a list: {}", arg), None))
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("first must take list argument. Found: {}", args), None))
}

fn rest(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(arg, _) = args {
        if let BLispExpr::SExp(_, rest) = *arg {
            return BLispEvalResult::Result(*rest)
        }
        return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("rest given argument: {}", arg), None))
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("rest must take list argument. Found: {}", args), None))
}

//=============== Numerical Operations ===============
fn incr(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(num), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Number(num + 1)),
            (BLispExpr::Float(float), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Float(float + 1.0)),
            (non_number, BLispExpr::Nil) => 
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("incr only takes a numerical argument. Found: {}", non_number), None)),
            (_, rest) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("incr only takes single argument. Found: {}", rest), None)),
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed argument list to incr. Found: {}", args), None))
}

fn decr(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(num), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Number(num - 1)),
            (BLispExpr::Float(float), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Float(float - 1.0)),
            (non_number, BLispExpr::Nil) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("decr only takes a numerical argument. Found: {}", non_number), None)),
            (_, rest) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("decr only takes single argument. Found: {}", rest), None)),
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed argument list to incr. Found: {}", args), None))
}

fn abs(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(num), BLispExpr::Nil) => BLispEvalResult::Result(BLispExpr::Number(num.abs())),
            (BLispExpr::Float(float), BLispExpr::Nil) => BLispEvalResult::Result(BLispExpr::Float(float.abs())),
            (non_number, BLispExpr::Nil) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("abs only takes a numerical argument. Found: {}", non_number), None)),
            (_, rest) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("abs only takes single argument. Found: {}", rest), None)),
        }
    } else {
        BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed argument list to abs. Found: {}", args), None))
    }
}

fn sign(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(num), BLispExpr::Nil) => BLispEvalResult::Result(BLispExpr::Number(num.signum())),
            (BLispExpr::Float(float), BLispExpr::Nil) => BLispEvalResult::Result(BLispExpr::Float(float.signum())),
            (non_number, BLispExpr::Nil) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("sign only takes a numerical argument. Found: {}", non_number), None)),
            (_, rest) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("sign only takes single argument. Found: {}", rest), None)),
        }
    } else {
        BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed argument list to abs. Found: {}", args), None))
    }
}

fn add(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispEvalResult::Result(BLispExpr::Number(first + second)),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Float(first + second as f64)),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first as f64 + second)),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first + second)),
                    (first, second) =>
                        return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Unexpected types to add. Found: {} {}", first, second), None))
                }
            }
        }
    }

    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Add expects two arguments. Found: {}", args), None))
}

fn sub(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispEvalResult::Result(BLispExpr::Number(first - second)),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Float(first - second as f64)),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first as f64 - second)),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first - second)),
                    (first, second) => BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Unexpected types to sub. Found: {} {}", first, second), None))
                }
            }
        }
    }

    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Add expects two arguments. Found: {}", args), None))
}

fn mul(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispEvalResult::Result(BLispExpr::Number(first * second)),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Float(first * second as f64)),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first as f64 * second)),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first * second)),
                    (first, second) => BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Unexpected types to mul. Found: {} {}", first, second), None))
                }
            }
        }
    }

    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Add expects two arguments. Found: {}", args), None))
}

fn int_div(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispEvalResult::Result(BLispExpr::Number(first / second)),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Float(first / second as f64)),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first as f64 / second)),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first / second)),
                    (first, second) => BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Unexpected types to int div. Found: {} {}", first, second), None))
                }
            }
        }
    }

    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Add expects two arguments. Found: {}", args), None))
}

fn div(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispEvalResult::Result(BLispExpr::Float(first as f64 / second as f64)),
                    (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Float(first / second as f64)),
                    (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first as f64 / second)),
                    (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Float(first / second)),
                    (first, second) => BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Unexpected types to div. Found: {} {}", first, second), None))
                }
            }
        }
    }

    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Add expects two arguments. Found: {}", args), None))
}

fn modulo(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        if let BLispExpr::SExp(second, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return match (*first, *second) {
                    (BLispExpr::Number(first), BLispExpr::Number(second))  => BLispEvalResult::Result(BLispExpr::Number(first % second)),
                    (first, second) => BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Unexpected types to modulo. Found: {} {}", first, second), None))
                }
            }
        }
    }

    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Add expects two arguments. Found: {}", args), None))
}

//=============== Logical Operators ===============
fn not(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Bool(value), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(!value)),
            (first, rest) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("not must take single, boolean argument ({} . {})", first, rest), None))
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed argument give to not. Found: {}", args), None))
}

fn and(args: BLispExpr, env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match evaluate(first, env.clone()) {
            BLispEvalResult::Result(BLispExpr::Bool(true)) => {
                match evaluate(second, env.clone()) {
                    BLispEvalResult::Result(BLispExpr::Bool(value)) => return BLispEvalResult::TailCall(BLispExpr::Bool(value), env.clone()),
                    BLispEvalResult::Result(second) =>
                        BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Second argument to \"and\" not boolean. Found: {}", second), None)),
                    BLispEvalResult::Error(error) => BLispEvalResult::Error(error),
                    BLispEvalResult::TailCall(_, _) => BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("TailCall found as second argument of and"), None)),
                }
            },
            BLispEvalResult::Result(BLispExpr::Bool(false)) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            BLispEvalResult::Result(first) =>
                BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("First argument to \"and\" not boolean. Found: {}", first), None)),
            BLispEvalResult::Error(error) => BLispEvalResult::Error(error),
            BLispEvalResult::TailCall(_, _) => BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("TailCall found as first argument of and"), None)),
        }
    })
}

fn or(args: BLispExpr, env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match evaluate(first, env.clone()) {
            BLispEvalResult::Result(BLispExpr::Bool(false)) => {
                match evaluate(second, env.clone()) {
                    BLispEvalResult::Result(BLispExpr::Bool(value)) => return BLispEvalResult::TailCall(BLispExpr::Bool(value), env.clone()),
                    BLispEvalResult::Result(result) =>
                        BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Second argument to \"or\" not boolean. Found: {}", result), None)),
                    BLispEvalResult::Error(error) => BLispEvalResult::Error(error),
                    BLispEvalResult::TailCall(_, _) => BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("TaillCall found as second argument of or"), None)),
                }
            },
            BLispEvalResult::Result(BLispExpr::Bool(true)) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            BLispEvalResult::Result(result) =>
                BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("First argument to \"or\" not boolean. Found: {}", result), None)),
            BLispEvalResult::Error(error) => BLispEvalResult::Error(error),
            BLispEvalResult::TailCall(_, _) => BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("TailCall found as first argument of or"), None)),
        }
    })
}

fn xor(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Bool(first), BLispExpr::Bool(second)) => return BLispEvalResult::Result(BLispExpr::Bool((first || second) && !(first && second))),
            (first, second) =>
                BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("\"and\" predicate must be given two boolean arguments. Found: {} {}", first, second), None)),
        }
    })
}

//=============== Bitwise Operators ===============
fn b_not(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(value), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Number(!value)),
            (first, second) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation,
                                                              format!("Bitwise not must recieve single Number as argument. Found: {} {}", first, second),
                                                              None))
        }
    }

    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to bitwise not. Found: {}", args), None))
}

fn b_and(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => return BLispEvalResult::Result(BLispExpr::Number(first & second)),
            (first, second) =>
                BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Bitwise and must take two Number arguments. Found: {} {}", first, second), None)),
        }
    })
}

fn b_or(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => return BLispEvalResult::Result(BLispExpr::Number(first | second)),
            (first, second) =>
                BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Bitwise or must take two Number arguments. Found: {} {}", first, second), None)),
        }
    })
}

fn b_xor(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => return BLispEvalResult::Result(BLispExpr::Number(first ^ second)),
            (first, second) =>
                BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Bitwise and must take two Number arguements. Found: {} {}", first, second), None))
        }
    })
}

//=============== Type Checks ===============
fn is_nil(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Nil, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Too many arguments provided to Nil check. Found: {} {}", first, rest), None)),
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to Nil check. Found: {}", args), None))
}

fn is_bool(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Bool(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Too many arguments provided to Bool check. Found: {} {}", first, rest), None)),
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to Bool check. Found: {}", args), None))
}

fn is_number(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Number(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Too many arguments provided to Number check. Found: {} {}", first, rest), None))
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to Number check. Found: {}", args), None))
}

fn is_float(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Float(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Too many arguments provided to Float check. Found: {} {}", first, rest), None))
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to Float check. Found: {}", args), None))
}

fn is_char(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Char(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Too many arguments provided to Char check. Found: {} {}", first, rest), None)),
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to Char check. Found: {}", args), None))
}

fn is_symbol(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Symbol(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Too many arguments provided to Symbol check. Found: {} {}", first, rest), None))
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to Symbol check. Found: {}", args), None))
}

fn is_applicable(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::SpecialForm(_), BLispExpr::Nil) |
            (BLispExpr::Function(_), BLispExpr::Nil) |
            (BLispExpr::Lambda(_, _, _), BLispExpr::Nil) |
            (BLispExpr::Macro(_, _), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Too many arguments provided to Applicable check. Found: {} {}", first, rest), None))
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to Applicable check. Found: {}", args), None))
}

fn is_special_form(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::SpecialForm(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Too many arguments provided to Special Form check. Found: {} {}", first, rest), None))
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to Special Form check. Found: {}", args), None))
}

fn is_function(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::Function(_), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Too many arguments provided to Function check. Found: {} {}", first, rest), None))
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to Function check. Found: {}", args), None))
}

fn is_list(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args {
        match (*first, *rest) {
            (BLispExpr::SExp(_, _), BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(true)),
            (_, BLispExpr::Nil) => return BLispEvalResult::Result(BLispExpr::Bool(false)),
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Too many arguments provided to List check. Found: {} {}", first, rest), None))
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to List check. Found: {}", args), None))
}
//=============== Comparison Predicates ===============
fn eq(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        BLispEvalResult::Result(BLispExpr::Bool(first == second))
    })
}

fn neq(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        BLispEvalResult::Result(BLispExpr::Bool(first != second))
    })
}

fn lt(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first < second)),
            (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first < second as f64)),
            (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool((first as f64) < second)),
            (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool(first < second)),
            (first, second) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Incompatible types given to less than. Found: {} {}", first, second), None)),
        }
    })
}

fn lte(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first <= second)),
            (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first <= second as f64)),
            (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool((first as f64) <= second)),
            (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool(first <= second)),
            (first, second) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Incompatible types given to less than. Found: {} {}", first, second), None)),
        }
    })
}

fn gt(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first > second)),
            (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first > second as f64)),
            (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool((first as f64) > second)),
            (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool(first > second)),
            (first, second) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Incompatible types given to less than. Found: {} {}", first, second), None)),
        }
    })
}

fn gte(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    apply_predicate(args, |first, second| {
        match (first, second) {
            (BLispExpr::Number(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first >= second)),
            (BLispExpr::Float(first), BLispExpr::Number(second)) => BLispEvalResult::Result(BLispExpr::Bool(first >= second as f64)),
            (BLispExpr::Number(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool((first as f64) >= second)),
            (BLispExpr::Float(first), BLispExpr::Float(second)) => BLispEvalResult::Result(BLispExpr::Bool(first >= second)),
            (first, second) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Incompatible types given to less than. Found: {} {}", first, second), None)),
        }
    })
}

//=============== Special Forms ===============
fn if_impl(args: BLispExpr, env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(predicate, rest) = args.clone() {
        if let BLispExpr::SExp(first, rest) = *rest {
            if let BLispExpr::SExp(second, rest) = *rest {
                if *rest == BLispExpr::Nil {
                    match evaluate(*predicate, env.clone()) {
                        BLispEvalResult::Result(BLispExpr::Bool(true)) => return BLispEvalResult::TailCall(*first, env),
                        BLispEvalResult::Result(BLispExpr::Bool(false)) => return BLispEvalResult::TailCall(*second, env),
                        BLispEvalResult::Result(result) =>
                            return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation,
                                                                          format!("First argument to \"if\" must evaluate to boolean. Found: {}", result),
                                                                          None)),
                        BLispEvalResult::Error(error) => return BLispEvalResult::Error(error),
                        BLispEvalResult::TailCall(_, _) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation,
                                                                                                         format!("TallCall found as result to if predicate"),
                                                                                                         None)),
                    }
                }
            }
        }
    }
    
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("if requires predicate and two more arguments. Found: {}", args), None))
}

fn bind_single_binding(binding: BLispExpr, eval_env: Rc<BLispEnv>, env: &mut BLispEnv) {
    if let BLispExpr::SExp(symbol, rest) = binding {
        if let BLispExpr::SExp(value, rest) = *rest {
            match (*symbol, *value, *rest) {
                (BLispExpr::Symbol(name), value, BLispExpr::Nil) => { 
                    match evaluate(value, eval_env) {
                        BLispEvalResult::Result(value) => env.insert(name, value),
                        BLispEvalResult::Error(error) => panic!("{}", error),
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

fn let_impl(args: BLispExpr, env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(binding_list, rest) = args.clone() {
        if let BLispExpr::SExp(expr, rest) = *rest {
            if *rest == BLispExpr::Nil {
                let mut child_env = BLispEnv::extend(env.clone());
                bind_from_binding_list(*binding_list, env.clone(), &mut child_env);
                return BLispEvalResult::TailCall(*expr, Rc::new(child_env))
            }
        }
    }

    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("let requires list of bindings and single expr to execute with bindings. Found: {}", args), None))
}

fn let_seq(args: BLispExpr, env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(binding_list, rest) = args.clone() {
        if let BLispExpr::SExp(expr, rest) = *rest {
            if *rest == BLispExpr::Nil {
                let child_env = BLispEnv::extend(env.clone());
                let child_env = bind_from_seq_binding_list(*binding_list, child_env);
                return BLispEvalResult::TailCall(*expr, Rc::new(child_env))
            }
        }
    }

    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("let* requires list of bindings and single expr to execute with bindings. Found: {}", args), None))
}

fn dyn_let(args: BLispExpr, env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
     if let BLispExpr::SExp(binding_expr, rest) = args.clone() {
        if let BLispExpr::SExp(expr, rest) = *rest {
            if *rest == BLispExpr::Nil {
                match evaluate(*binding_expr, env.clone()) {
                    BLispEvalResult::Result(binding_list) => {
                        let child_env = BLispEnv::extend(env.clone());
                        let child_env = bind_from_seq_binding_list(binding_list, child_env);
                        return BLispEvalResult::TailCall(*expr, Rc::new(child_env))
                    },
                    BLispEvalResult::Error(error) => return BLispEvalResult::Error(error),
                    BLispEvalResult::TailCall(_, _) =>
                        return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("TailCall found as result of dyn-let binding"), None))
                }
            }
        }
    }

    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("dyn-let requires list of bindings and single expr to execute with bindings. Found: {}", args), None))
}

fn load(args: BLispExpr, env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(file_name, rest) = args.clone() {
        match (*file_name, *rest) {
            (file_name, BLispExpr::Nil) => {
                let loaded = fs::read_to_string(collect_string(file_name)).expect("Failed to open file").parse().expect("Failed to read file");
                match blisp_lexer::lex(loaded) {
                    Ok(mut list) => 
                        match blisp_parser::parse(&mut list) {
                            Ok(ast) => return BLispEvalResult::TailCall(ast, env.clone()),
                            Err(error) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("{}", error), None))
                        }
                    Err(error) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("{}", error), None))
                }
            },
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Load must take single filename argument. Found: {} {}", first, rest), None))
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list passed to load. Found: {}", args), None))
}

//=============== Lambda Creation ===============
fn lambda(args: BLispExpr, env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(binding_list, rest) = args.clone() {
        if let BLispExpr::SExp(eval_expr, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return BLispEvalResult::Result(BLispExpr::Lambda(binding_list, eval_expr, env));
            }
        }
    }

    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed arguments given to lambda. Found: {}", args), None))
}

//=============== Input/Output ===============
fn print_std(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        if *rest == BLispExpr::Nil {
            println!("{}", collect_string(*first));
            return BLispEvalResult::Result(BLispExpr::Bool(true));
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to print. Found: {}", args), None))
}

fn debug(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        if *rest == BLispExpr::Nil {
            println!("{}", first);
            return BLispEvalResult::Result(BLispExpr::Bool(true));
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed argument given to debug. Found: {}", args), None))
}

fn read_std(_args: BLispExpr, env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).expect("Failed to read from standard in");
    buffer.pop();
    buffer = format!("\"{}\"", buffer);
    match blisp_lexer::lex(buffer.chars().collect()) {
        Ok(mut list) => 
            match parse_string_literal(&mut list) {
                Ok(parsed) => BLispEvalResult::TailCall(parsed, env.clone()),
                Err(error) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("{}", error), None))
            }
        Err(error) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("{}", error), None))
    }
}

//=============== Macros ===============
pub fn quote(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        match (*first, *rest) {
            (first, BLispExpr::Nil) => BLispEvalResult::Result(first),
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("quote must take single argument. Found: {} {}", first, rest), None))
        }
    } else {
        BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to quote. Found: {}", args), None))
    }
}

pub fn quasiquote(args: BLispExpr, env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    fn quasi_list_item(args: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
        if let BLispExpr::SExp(first, arg_list) = args.clone() {
            if *first == BLispExpr::Symbol(String::from("unquote")) {
                if let BLispExpr::SExp(arg, nil) = *arg_list {
                    if *nil == BLispExpr::Nil {
                        return evaluate(*arg, env.clone());
                    }
                }
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("unquote must take single argument. Found: {}", args), None))
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
                        (_, BLispEvalResult::TailCall(_, _)) =>
                            return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("TailCall returned as result of quasiquoted list"), None))
                }
            }
        } else {
            quasi_list_item(args, env.clone())
        }
    }

    if let BLispExpr::SExp(first, rest) = args.clone() {
        match (*first, *rest) {
            (first, BLispExpr::Nil) => {
                match quasi_list(first, env) {
                    BLispEvalResult::Result(item) => return BLispEvalResult::Result(item),
                    BLispEvalResult::Error(error) => return BLispEvalResult::Error(error),
                    BLispEvalResult::TailCall(_, _) =>
                        return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("TailCall returned as a result of quasiquoted list"), None))
                }
            },
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Quasiquote can only take single argument. Found: {} {}", first, rest), None))
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to quasiquote. Found: {}", args), None))
}

fn def_macro(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(binding_list, rest) = args.clone() {
        if let BLispExpr::SExp(eval_expr, rest) = *rest {
            if *rest == BLispExpr::Nil {
                return BLispEvalResult::Result(BLispExpr::Macro(binding_list, eval_expr));
            }
        }
    }

    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed arguments given to macro. Found: {}", args), None))
}

fn eval(args: BLispExpr, env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        match (*first, *rest) {
            (first, BLispExpr::Nil) => BLispEvalResult::TailCall(first, env),
            (first, rest) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("eval must take single argument. Found: {} {}", first, rest), None))
        }
    } else {
        BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to quote. Found: {}", args), None))
    }
}

fn exec(args: BLispExpr, env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        match (*first, *rest) {
            (first, BLispExpr::Nil) => {
                let string = collect_string(first);
                match blisp_lexer::lex(string) {
                    Ok(mut list) => 
                        match blisp_parser::parse(&mut list) {
                            Ok(ast) => return BLispEvalResult::TailCall(ast, env.clone()),
                            Err(error) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("{}", error), None))
                        }
                    Err(error) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("{}", error), None))
                }
            },
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Single argument must be passed to exec. Found: {} {}", first, rest), None))
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed string given to exec. Found: {}", args), None))
}

fn apply(args: BLispExpr, env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    fn quote_each_elt(list: BLispExpr) -> BLispEvalResult {
        if let BLispExpr::SExp(first, rest) = list {
            match quote_each_elt(*rest) { 
                BLispEvalResult::Result(rest) => 
                    return BLispEvalResult::Result(BLispExpr::cons_sexp(BLispExpr::cons_sexp(BLispExpr::SpecialForm(quote), BLispExpr::cons_sexp(*first, BLispExpr::Nil)),
                                                                        rest)),
                BLispEvalResult::Error(error) => return BLispEvalResult::Error(error),
                BLispEvalResult::TailCall(expr, _) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("TailCall found while applying: {}", expr), None)),
            }
        } else if list == BLispExpr::Nil {
            return BLispEvalResult::Result(BLispExpr::Nil);
        } else {
            return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Unexpected expr while applying list: {}", list), None));
        }
    }

    if let BLispExpr::SExp(applicable, rest) = args.clone() {
        if let BLispExpr::SExp(list, nil) = *rest {
            if *nil != BLispExpr::Nil {
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Too many arguments provided to apply: {}", *nil), None));
            }

            match quote_each_elt(*list) {
                BLispEvalResult::Result(list) => return BLispEvalResult::TailCall(BLispExpr::cons_sexp(*applicable, list), env.clone()),
                BLispEvalResult::Error(error) => return BLispEvalResult::Error(error),
                BLispEvalResult::TailCall(expr, _) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("TailCall found while applying: {}", expr), None)),
            }
        }
    }

    return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed list given to apply: {}", args), None));
}

fn parse(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(first, rest) = args.clone() {
        match (*first, *rest) {
            (first, BLispExpr::Nil) => {
                let string = collect_string(first);
                match blisp_lexer::lex(string) {
                    Ok(mut list) => 
                        match blisp_parser::parse(&mut list) {
                            Ok(ast) => return BLispEvalResult::Result(ast),
                            Err(error) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("{}", error), None))
                        }
                    Err(error) => return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("{}", error), None))
                }
            },
            (first, rest) =>
                return BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Single argument must be passed to exec. Found: {} {}", first, rest), None))
        }
    }
    BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Malformed string given to exec. Found: {}", args), None))
}

//=============== Continuation Handling ===============
pub fn call_with_current_continuation(args: BLispExpr, env: Rc<BLispEnv>, cc: BLispCallStack) -> BLispEvalResult {
    let continuation = BLispExpr::Continuation(cc.clone());
    let cont_args = BLispExpr::cons_sexp(continuation, BLispExpr::Nil);
    if let BLispExpr::SExp(applicable, rest) = args.clone() {
        match (*applicable, *rest) {
            (applicable, BLispExpr::Nil) => BLispEvalResult::TailCall(BLispExpr::cons_sexp(applicable, cont_args), env),
            (_, unexpected) => BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Too many arguments given to call/cc: {}", unexpected), None)),
        }
    } else {
        BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Unexpected argument format passed to call/cc: {}", args), None))
    }
}

pub fn reset(args: BLispExpr, _env: Rc<BLispEnv>, _cc: BLispCallStack) -> BLispEvalResult {
    if let BLispExpr::SExp(arg, nil) = args {
        if *nil == BLispExpr::Nil {
            BLispEvalResult::Result(*arg)
        } else {
            panic!("Too many arguments provided to reset: {}", *nil)
        }
    } else {
        panic!("Malformed argument list provided to reset: {}", args)
    }
}

pub fn shift(args: BLispExpr, env: Rc<BLispEnv>, cc: BLispCallStack) -> BLispEvalResult {
    fn is_frame_reset(frame: &BLispFrame) -> bool {
        if let Some(first) = frame.eval_buffer.first() {
            *first == BLispExpr::Function(reset)
        } else {
            false
        }
    }

    if let BLispExpr::SExp(arg, nil) = args {
        if *nil == BLispExpr::Nil {
            let mut frame_list: Vec<BLispFrame> = vec![];
            if let Some(mut cur_node) = cc.parent() {
                while let Some(cur_frame) = cur_node.val() {
                    if is_frame_reset(cur_frame) {
                        break;
                    } else {
                        frame_list.push(cur_frame.clone());
                    }

                    if let Some(parent_node) = cur_node.parent() {
                        cur_node = parent_node;
                    } else {
                        break;
                    }
                }
                let delimited_continuation = BLispExpr::DelimitedContinuation(frame_list);
                let delimited_application = BLispExpr::cons_sexp(*arg, BLispExpr::cons_sexp(delimited_continuation, BLispExpr::Nil));
                let reset_continuation = BLispExpr::Continuation(cur_node.clone());
                let reset_call = BLispExpr::cons_sexp(reset_continuation, BLispExpr::cons_sexp(delimited_application, BLispExpr::Nil));
                BLispEvalResult::TailCall(reset_call, env)
            } else {
                panic!("Shift has no parent call stack frame")
            }
        } else {
            panic!("Too many arguments provided to shift: {}", *nil)
        }
    } else {
        panic!("Malformed argument list provided to shift: {}", args)
    }
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
    env.insert("apply".to_string(), BLispExpr::Function(apply));
    env.insert("quote".to_string(), BLispExpr::SpecialForm(quote));
    env.insert("quasiquote".to_string(), BLispExpr::SpecialForm(quasiquote));
    env.insert("unquote".to_string(), BLispExpr::Function(eval));
    env.insert("exec".to_string(), BLispExpr::Function(exec));
    env.insert("parse".to_string(), BLispExpr::Function(parse));

    env.insert("exit".to_string(), BLispExpr::Function(exit));

    env.insert("print".to_string(), BLispExpr::Function(print_std));
    env.insert("debug".to_string(), BLispExpr::Function(debug));
    env.insert("read".to_string(), BLispExpr::Function(read_std));

    env.insert("call-with-current-continuation".to_string(), BLispExpr::Function(call_with_current_continuation));
    env.insert("call/cc".to_string(), BLispExpr::Function(call_with_current_continuation));
    env.insert("reset".to_string(), BLispExpr::Function(reset));
    env.insert("shift".to_string(), BLispExpr::SpecialForm(shift));

    env
}

