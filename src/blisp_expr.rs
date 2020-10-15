use std::collections::HashMap;
use std::rc::Rc;

type BLispEnvMap = HashMap<String, BLispExpr>;

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
