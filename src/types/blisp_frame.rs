use std::rc::Rc;

use cactus::Cactus;

use crate::types::{
    BLispExpr,
    BLispEnv,
};

pub type BLispCallStack = Cactus<BLispFrame>;

pub fn format_call_stack(mut stack: BLispCallStack) ->  String {
    let mut formatted_stack: Vec<String> = vec![];
    while let Some(frame) = stack.val() {
        formatted_stack.push(format!("{}{}", if let BLispFrameType::Continuation(Some(label)) = &frame.frame_type { format!("{}: ", label) } else { format!("") }, frame));
        if let Some(parent) = stack.parent() {
            stack = parent;
        } else {
            break;
        }
    }

    let mut formatted_string = format!("Continuation:\n");
    for string in formatted_stack.iter().rev() {
        formatted_string = format!("{}Frame:\n\t{}\n", formatted_string, string);
    }
    formatted_string
}

#[derive(Clone, Debug, PartialEq)]
pub enum BLispFrameType {
    Anonymous,
    Continuation(Option<String>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct BLispFrame {
    pub expr: BLispExpr,
    pub eval_buffer: Vec<BLispExpr>,
    pub env: Rc<BLispEnv>,
    pub frame_type: BLispFrameType
}

impl BLispFrame {
    pub fn new(expr: BLispExpr, eval_buffer: Vec<BLispExpr>, env: Rc<BLispEnv>) -> BLispFrame {
        BLispFrame { expr, eval_buffer, env, frame_type: BLispFrameType::Anonymous }
    }

    pub fn new_cont(expr: BLispExpr, eval_buffer: Vec<BLispExpr>, env: Rc<BLispEnv>, cont_tag: Option<String>) -> BLispFrame {
        BLispFrame { expr, eval_buffer, env, frame_type: BLispFrameType::Continuation(cont_tag) }
    }

    pub fn list_step(&self, nexpr: BLispExpr) -> BLispFrame {
        let mut new_frame = self.clone();
        match new_frame.expr {
            BLispExpr::SExp(_, rest) => {
                new_frame.expr = *rest;
            },
            unexpected => panic!("Given non-list expr frame to step on: {} for {}", unexpected, self.expr),
        }

        new_frame.eval_buffer.push(nexpr);

        new_frame
    }

    pub fn matches_continuation(&self, tag: Option<String>) -> bool {
        match &self.frame_type {
            BLispFrameType::Anonymous => false,
            BLispFrameType::Continuation(self_tag) => *self_tag == tag || tag == None,
        }
    }
}

impl std::fmt::Display for BLispFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let temp_expr = self.expr.clone();
        let mut buffer_expr = BLispExpr::Nil;
        for expr in self.eval_buffer.iter().rev() {
            buffer_expr = BLispExpr::cons_sexp(expr.clone(), buffer_expr);
        }
        write!(f, "{}...\n\t -> {}", buffer_expr, temp_expr)
    }
}

