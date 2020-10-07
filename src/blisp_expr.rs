use std::collections::HashMap;

type BLispEnvMap = HashMap<String, BLispExpr>;

struct BLispEnv {
    map: BLispEnvMap,
}

impl BLispEnv {
    pub fn new() -> BLispEnv {
        BLispEnv { map: BLispEnvMap::new() }
    }
}


#[derive(Debug, PartialEq)]
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

impl BLispExpr {
    pub fn is_disallowed_symbol_char(character: char) -> bool {
        "()[]{}\\/\"\'#".contains(character)
    }
}
