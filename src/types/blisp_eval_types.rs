use std::rc::Rc;

use crate::types::{
    BLispExpr,
    BLispEnv,
    BLispError,
    BLispCallStack,
    format_call_stack,
};

#[derive(Debug, PartialEq, Clone)]
pub enum BLispEvalResult {
    Result(BLispExpr),
    TailCall(BLispExpr, Rc<BLispEnv>),
    Stack(BLispCallStack),
    Error(BLispError),
}

impl std::fmt::Display for BLispEvalResult {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BLispEvalResult::Result(expr) => write!(f, "Result({})", expr),
            BLispEvalResult::Error(error) => write!(f, "{}", error),
            BLispEvalResult::TailCall(expr, _) => write!(f, "TailCall({})", expr),
            BLispEvalResult::Stack(stack) => write!(f, "Stack({})", format_call_stack(stack.clone())),
        }
    }
}

