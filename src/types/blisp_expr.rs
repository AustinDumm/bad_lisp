use std::rc::Rc;

use crate::types::{
    BLispEnv,
    BLispEvalResult,
    BLispError,
};

pub type BLispExprResult = Result<BLispExpr, BLispError>;

#[derive(Debug, PartialEq, Clone)]
pub enum BLispExpr {
    Nil,
    Bool(bool),
    Number(i64),
    Float(f64),
    Char(char),
    Symbol(String),
    SpecialForm(fn(BLispExpr, Rc<BLispEnv>) -> BLispEvalResult),
    Function(fn(BLispExpr, Rc<BLispEnv>) -> BLispEvalResult),
    Lambda(Box<BLispExpr>, Box<BLispExpr>, Rc<BLispEnv>),
    Macro(Box<BLispExpr>, Box<BLispExpr>),
    SExp(Box<BLispExpr>, Box<BLispExpr>),
}

impl std::fmt::Display for BLispExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BLispExpr::Nil => write!(f, "()"),
            BLispExpr::Bool(value) => write!(f, "{}", if *value { "#t" } else { "#f" }),
            BLispExpr::Number(value) => write!(f, "{}", value),
            BLispExpr::Float(value) => write!(f, "{}", value),
            BLispExpr::Char(value) => write!(f, "{}", value),
            BLispExpr::Symbol(name) => write!(f, "{}", name),
            BLispExpr::SExp(_, _) => write!(f, "({})", self.format_as_list()),
            BLispExpr::SpecialForm(_) => write!(f, "SpecialForm"),
            BLispExpr::Function(_) => write!(f, "Function"),
            BLispExpr::Macro(args, body) => write!(f, "Macro({} -> \n\t{})", args, body),
            BLispExpr::Lambda(args, body, _) => write!(f, "Lambda({} -> \n\t{})", args, body),
        }
    }
}

impl BLispExpr {
    pub fn is_disallowed_symbol_char(character: char) -> bool {
        "()[]{}\\\"\'#".contains(character)
    }

    pub fn cons_sexp(first: BLispExpr, rest: BLispExpr) -> BLispExpr {
        BLispExpr::SExp(Box::new(first),
                        Box::new(rest))
    }

    fn format_as_list(&self) -> String {
        if let BLispExpr::SExp(first, rest) = self {
            match (*first.clone(), *rest.clone()) {
                (value, BLispExpr::SExp(_, _)) => format!("{} {}", value, rest.format_as_list()),
                (value, BLispExpr::Nil) => format!("{}", value),
                (value, rest) => format!("{} . {}", value, rest),
            }
        } else {
            panic!("BLispExpr expected to be list")
        }
    }

    pub fn is_eval_applicable(&self) -> bool {
        match self {
            BLispExpr::Function(_)
                | BLispExpr::Lambda(_, _, _) => true,
            _ => false,
        }
    }

    pub fn is_noeval_applicable(&self) -> bool {
        match self {
            BLispExpr::SpecialForm(_)
                | BLispExpr::Macro(_, _) => true,
            _ => false
        }
    }
}
