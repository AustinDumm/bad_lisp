
use std::rc::Rc;

use crate::types::{
    BLispExpr,
    BLispEvalResult,
    BLispEnv,
    BLispError,
    BLispErrorType,
    BLispFrame,
    BLispCallStack,
};

#[derive(Clone)]
enum EvalFrameResult {
    Stack(BLispCallStack),
    EvalResult(BLispEvalResult),
}

pub fn evaluate(expr: BLispExpr, env: Rc<BLispEnv>) -> BLispEvalResult {
    let root = BLispCallStack::new();
    let mut stack = root.child(BLispFrame::new(expr, vec![], env.clone()));
    
    loop {
        match stack_eval_step(stack) {
            EvalFrameResult::Stack(new_stack) => stack = new_stack,
            EvalFrameResult::EvalResult(result) => return result,
        }
    }
}

fn stack_eval_step(stack: BLispCallStack) -> EvalFrameResult {
    if let Some(frame) = stack.val() {
        match frame.expr.clone() {
            BLispExpr::SExp(_, _) =>
                stack_eval_list_elts_step(stack),
            BLispExpr::Nil if frame.eval_buffer.len() > 0 => {
                let args = collect_eval_buffer(&frame.eval_buffer);
                stack_eval_list(stack, args)
            }
            BLispExpr::Symbol(sym_text) =>
                match frame.env.clone().get(&sym_text) {
                    None => EvalFrameResult::EvalResult(BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, format!("Failed symbol lookup: {}", sym_text), None))),
                    Some(expr) => stack_return(stack, expr.clone()),
                }
            expr =>
                stack_return(stack, expr),
        }
    } else {
        panic!("Empty frame found. Aborting evaluation")
    }
}

fn stack_return(stack: BLispCallStack, expr: BLispExpr) -> EvalFrameResult {
    if let Some(parent_node) = stack.parent() {
        if let Some(parent_frame) = parent_node.val() {
            if let Some(grandparent_node) = parent_node.parent() {
                let new_parent_frame = parent_frame.list_step(expr);
                EvalFrameResult::Stack(grandparent_node.child(new_parent_frame))
            } else {
                panic!("No grandparent node found on frame return")
            }
        } else {
            EvalFrameResult::EvalResult(BLispEvalResult::Result(expr))
        }
    } else {
        panic!("No parent node found on frame return")
    }
}

fn stack_tail_sub(stack: BLispCallStack, expr: BLispExpr, env: Rc<BLispEnv>) -> EvalFrameResult {
    if let Some(parent_node) = stack.parent() {
        let new_frame = BLispFrame::new(expr, vec![], env);
        EvalFrameResult::Stack(parent_node.child(new_frame))
    } else {
        panic!("No parent on which to substitute current node")
    }
}

fn stack_eval_next_elt(stack: BLispCallStack) -> EvalFrameResult {
    if let Some(frame) = stack.val() {
        match frame.expr.clone() {
            BLispExpr::SExp(first, _) => EvalFrameResult::Stack(stack.child(BLispFrame::new(*first.clone(), vec![], frame.env.clone()))),
            unexpected => panic!("Non-list passed to stack_eval_list_elts_step: {}", unexpected),
        }
    } else {
        panic!("Empty frame given to stack_eval_next_elt")
    }
}

fn stack_eval_list_elts_step(stack: BLispCallStack) -> EvalFrameResult {
    if let Some(frame) = stack.val() {
        match frame.eval_buffer.len() {
            0 => stack_eval_next_elt(stack),
            _ => {
                match frame.eval_buffer.first().expect("No front found on evaluation list") {
                    first if first.is_eval_applicable() => stack_eval_next_elt(stack),
                    first if first.is_noeval_applicable() => {
                        let pair = (first.clone(), frame.expr.clone());
                        stack_eval_list(stack, pair)
                    }
                    unexpected => panic!("Unapplicable element as beginning of list: {}", unexpected),
                }
            },
        }
    } else {
        panic!("Empty stack node given to elts eval")
    }
}

fn collect_eval_buffer(buffer: &Vec<BLispExpr>) -> (BLispExpr, BLispExpr) {
    let mut expr = BLispExpr::Nil;
    for next_expr in buffer.iter().rev() {
        expr = BLispExpr::cons_sexp(next_expr.clone(), expr);
    }

    match expr {
        BLispExpr::SExp(operation, args) => (*operation, *args),
        _ => unreachable!(),
    }
}

fn apply_frames_to_stack(mut stack: BLispCallStack, frame_list: Vec<BLispFrame>) -> BLispCallStack {
    for frame in frame_list.iter().rev() {
        stack = stack.child(frame.clone());
    }

    stack
}

fn stack_eval_list(stack: BLispCallStack, operation_args: (BLispExpr, BLispExpr)) -> EvalFrameResult {
    if let Some(frame) = stack.val() {
        match operation_args {
            (BLispExpr::Function(func), args) |
            (BLispExpr::SpecialForm(func), args) => {
                match func(args, frame.env.clone(), stack.clone()) {
                    BLispEvalResult::Result(result) => stack_return(stack, result),
                    BLispEvalResult::TailCall(result, env) => stack_tail_sub(stack, result, env),
                    BLispEvalResult::Error(error) => EvalFrameResult::EvalResult(BLispEvalResult::Error(error)),
                }
            },
            (BLispExpr::Lambda(args_names, body, env), args) => {
                match BLispEnv::bind(env, *args_names, args) {
                    Ok(env) => stack_tail_sub(stack, *body, Rc::new(env)),
                    Err(error) => EvalFrameResult::EvalResult(BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, error, None))),
                }
            },
            (BLispExpr::Macro(args_names, body), args) => {
                match BLispEnv::bind(frame.env.clone(), *args_names, args) {
                    Ok(env) => stack_tail_sub(stack, *body, Rc::new(env)),
                    Err(error) => EvalFrameResult::EvalResult(BLispEvalResult::Error(BLispError::new(BLispErrorType::Evaluation, error, None))),
                }
            },
            (BLispExpr::Continuation(stack), args) => {
                if let BLispExpr::SExp(first, nil) = args {
                    match (*first, *nil) {
                        (arg, BLispExpr::Nil) => stack_return(stack, arg),
                        (arg, unexpected) => panic!("Too many arguments provided to continuation: ({}, {})", arg, unexpected),
                    }
                } else {
                    panic!("Malformed arguments given to continuation")
                }
            },
            (BLispExpr::DelimitedContinuation(frame_list), args) => {
                if let BLispExpr::SExp(first, nil) = args {
                    match (*first, *nil, stack.parent()) {
                        (arg, BLispExpr::Nil, Some(stack)) => stack_return(apply_frames_to_stack(stack, frame_list).child(BLispFrame::new(BLispExpr::Nil, vec![], std::rc::Rc::new(BLispEnv::new()))), arg),
                        (_, _, None) => panic!("No parent node found on delimited continuation application"),
                        (_, unexpected, _) => panic!("Too many arguments provided to delimited continuation: {}", unexpected),
                    }
                } else {
                    panic!("Malformed arguments given to delimited continuation")
                }
            }
            _ => panic!("Unapplicable element used as operation"),
        }
    } else {
        panic!("Empty node given to stack_eval_list")
    }
}

#[cfg(test)]
mod blisp_eval_tests {
    use super::*;
    use crate::blisp_func::default_env;

    #[test]
    fn evaluates_simple_lists() {
        assert_eq!(
            evaluate(
                BLispExpr::cons_sexp(
                    BLispExpr::Symbol("cons".to_string()),
                    BLispExpr::cons_sexp(
                        BLispExpr::Number(5),
                        BLispExpr::cons_sexp(
                            BLispExpr::cons_sexp(
                                BLispExpr::Symbol("cons".to_string()),
                                BLispExpr::cons_sexp(
                                    BLispExpr::Number(6),
                                    BLispExpr::cons_sexp(
                                        BLispExpr::Nil,
                                        BLispExpr::Nil
                                    )
                                )
                            ),
                            BLispExpr::Nil
                        )
                    )
                ),
                Rc::new(default_env())
            ),
            BLispExpr::cons_sexp(
                BLispExpr::Number(5),
                BLispExpr::cons_sexp(
                    BLispExpr::Number(6),
                    BLispExpr::Nil
                )
            )
        );

        assert_eq!(
            evaluate(
                BLispExpr::cons_sexp(
                    BLispExpr::Symbol("+".to_string()),
                    BLispExpr::cons_sexp(
                        BLispExpr::Number(5),
                        BLispExpr::cons_sexp(
                            BLispExpr::Number(6),
                            BLispExpr::Nil
                        )
                    )
                ),
                Rc::new(default_env())
            ),
            BLispExpr::Number(11)
        );
    }
}
