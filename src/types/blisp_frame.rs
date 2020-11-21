use std::rc::Rc;

use cactus::Cactus;

use crate::types::{
    BLispExpr,
    BLispEnv,
};

pub type BLispCallStack = Cactus<BLispFrame>;

#[derive(Clone, Debug, PartialEq)]
pub struct BLispFrame {
    pub expr: BLispExpr,
    pub eval_buffer: Vec<BLispExpr>,
    pub env: Rc<BLispEnv>,
}

impl BLispFrame {
    pub fn new(expr: BLispExpr, eval_buffer: Vec<BLispExpr>, env: Rc<BLispEnv>) -> BLispFrame {
        BLispFrame { expr, eval_buffer, env }
    }

    pub fn list_step(&self, nexpr: BLispExpr) -> BLispFrame {
        let mut new_frame = self.clone();
        match new_frame.expr {
            BLispExpr::SExp(_, rest) => {
                new_frame.expr = *rest;
            },
            unexpected => panic!("Given non-list expr frame to step on: {}", unexpected),
        }

        new_frame.eval_buffer.push(nexpr);

        new_frame
    }
}

