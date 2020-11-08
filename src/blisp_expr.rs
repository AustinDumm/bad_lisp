use std::collections::HashMap;
use std::rc::Rc;
use crate::blisp_lexer::BLispError;

type BLispEnvMap = HashMap<String, BLispExpr>;

#[derive(Debug, PartialEq)]
pub struct BLispEnv {
    map: BLispEnvMap,
    parent: Option<Rc<BLispEnv>>,
}

impl std::fmt::Display for BLispEnv {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }

}

impl BLispEnv {
    pub fn new() -> BLispEnv {
        BLispEnv { map: BLispEnvMap::new(), parent: None }
    }

    pub fn extend(parent: Rc<Self>) -> BLispEnv {
        BLispEnv { map: BLispEnvMap::new(), parent: Some(parent) }
    }

    pub fn insert(&mut self, key: String, value: BLispExpr) {
        self.map.insert(key, value);
    }

    pub fn get(&self, key: &String) -> Option<&BLispExpr> {
        match (self.map.get(key), &self.parent) {
            (None, Some(parent)) => parent.get(key),
            (result, _) => result,
        }
    }

    pub fn bind(parent: Rc<Self>, mut args_list: BLispExpr, mut args: BLispExpr) -> BLispEnv {
        let mut new_env = BLispEnv::extend(parent);

        loop {
            match (args_list, args) {
                (BLispExpr::Nil, BLispExpr::Nil) => break,
                (BLispExpr::Nil, _) => panic!("Too many arguments to bind"),
                (_, BLispExpr::Nil) => panic!("Too few arguments to bind"),
                (names, values) => {
                    if let (BLispExpr::SExp(name, name_rest), BLispExpr::SExp(value, value_rest)) = (names, values) {
                        match (*name, *value) {
                            (BLispExpr::Symbol(name), value) => {
                                args_list = *name_rest;
                                args = *value_rest;
                                new_env.insert(name, value)
                            },
                            (_, _) => panic!("Argument names must be of type Symbol"),
                        }
                    } else {
                        panic!("Malformed argument binding given bind");
                    }
                }
            }
        }

        new_env
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BLispEvalResult {
    Result(BLispExpr),
    TailCall(BLispExpr, Rc<BLispEnv>),
    Error(BLispError),
}

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
}
