use std::collections::HashMap;
use std::rc::Rc;

use crate::types::{
    BLispExpr,
};

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

    pub fn bind(parent: Rc<Self>, mut args_list: BLispExpr, mut args: BLispExpr) -> Result<BLispEnv, String> {
        let mut new_env = BLispEnv::extend(parent);

        loop {
            match (args_list, args) {
                (BLispExpr::Nil, BLispExpr::Nil) => break,
                (BLispExpr::Nil, bind) => return Err(format!("Too many arguments to bind: {}", bind)),
                (names, values) => {
                    match (names, values) {
                        (BLispExpr::SExp(name, _), BLispExpr::Nil) => return Err(format!("Too few arguments to bind: {}", name)),
                        (BLispExpr::SExp(name, name_rest), BLispExpr::SExp(value, value_rest)) => {
                            match (*name, *value) {
                                (BLispExpr::Symbol(name), value) => {
                                    args_list = *name_rest;
                                    args = *value_rest;
                                    new_env.insert(name, value)
                                },
                                (_, _) => panic!("Argument names must be of type Symbol"),
                            }
                        },
                        (BLispExpr::Symbol(symbol_text), remaining) => {
                            new_env.insert(symbol_text, remaining);
                            break;
                        },
                        (_, _) => panic!("Malformed argument binding given to bind"),
                    }
                }
            }
        }

        Ok(new_env)
    }
}
