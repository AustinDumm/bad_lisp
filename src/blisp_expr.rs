use std::collections::HashMap;

type BLispEnvMap = HashMap<String, BLispExpr>;

pub struct BLispEnv {
    map: BLispEnvMap,
}

impl BLispEnv {
    pub fn new() -> BLispEnv {
        BLispEnv { map: BLispEnvMap::new() }
    }

    pub fn insert(&mut self, key: String, value: BLispExpr) {
        self.map.insert(key, value);
    }

    pub fn get(&self, key: &String) -> Option<&BLispExpr> {
        self.map.get(key)
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
    Function(fn(BLispExpr) -> BLispExpr),
    SExp(Box<BLispExpr>, Box<BLispExpr>),
}

impl std::fmt::Display for BLispExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl BLispExpr {
    pub fn is_disallowed_symbol_char(character: char) -> bool {
        "()[]{}\\/\"\'#".contains(character)
    }

    pub fn cons_sexp(first: BLispExpr, rest: BLispExpr) -> BLispExpr {
        BLispExpr::SExp(Box::new(first),
                        Box::new(rest))
    }
}
