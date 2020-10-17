use std::collections::HashMap;
use std::rc::Rc;

type BLispEnvMap = HashMap<String, BLispExpr>;

#[derive(Debug, PartialEq)]
pub struct BLispEnv {
    map: BLispEnvMap,
    parent: Option<Rc<BLispEnv>>,
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
pub enum BLispExpr {
    Nil,
    Bool(bool),
    Number(i64),
    Float(f64),
    Char(char),
    Symbol(String),
    SpecialForm(fn(BLispExpr, Rc<BLispEnv>) -> BLispExpr),
    Function(fn(BLispExpr, Rc<BLispEnv>) -> BLispExpr),
    Lambda(Box<BLispExpr>, Box<BLispExpr>, Rc<BLispEnv>),
    SExp(Box<BLispExpr>, Box<BLispExpr>),
}

impl std::fmt::Display for BLispExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
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
}
