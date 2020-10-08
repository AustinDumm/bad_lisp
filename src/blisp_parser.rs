
use std::collections::VecDeque;

use crate::blisp_expr::{
    BLispExpr,
};

use crate::blisp_lexer::{
    BLispToken,
    BLispBrace,
};

pub fn parse(token_queue: &mut VecDeque<BLispToken>) -> BLispExpr {
    match token_queue.pop_front() {
        Some(BLispToken::OpenDelimiter(brace_type)) => parse_list(brace_type, token_queue),
        Some(BLispToken::Expr(expr)) => expr,
        Some(BLispToken::StringLiteral(string)) => parse_string_literal(string.iter()),
        _ => panic!("Unexpected token found while parsing"),
    }
}

pub fn parse_list(brace_type: BLispBrace, queue: &mut VecDeque<BLispToken>) -> BLispExpr {
    let contents = parse_list_contents(queue);
    if let Some(BLispToken::CloseDelimiter(close_brace)) = queue.pop_front() {
        if close_brace == brace_type {
            return contents
        }
        panic!("Mismatched brace types");
    }
    panic!("Unexpected end to parsing list");
}

pub fn parse_list_contents(queue: &mut VecDeque<BLispToken>) -> BLispExpr {
    match queue.pop_front() {
        Some(BLispToken::SExpDot) => parse(queue),
        Some(BLispToken::OpenDelimiter(brace_type)) => BLispExpr::cons_sexp(parse_list(brace_type, queue),
                                                                            parse_list_contents(queue)),
        Some(BLispToken::CloseDelimiter(brace_type)) => { queue.push_front(BLispToken::CloseDelimiter(brace_type)); BLispExpr::Nil },
        Some(BLispToken::Expr(expr)) => BLispExpr::cons_sexp(expr,
                                                             parse_list_contents(queue)),
        Some(BLispToken::StringLiteral(string)) => BLispExpr::cons_sexp(parse_string_literal(string.iter()),
                                                                        parse_list_contents(queue)),
        None => panic!("Unexpected end to token stream in list")
    }
}
    
pub fn parse_string_literal<'a, I>(mut char_iter: I) -> BLispExpr
where I: Iterator<Item = &'a char> {
    match char_iter.next() {
        Some(character) => BLispExpr::cons_sexp(BLispExpr::Char(*character),
                                                parse_string_literal(char_iter)),
        None => BLispExpr::Nil,
    }
}

#[cfg(test)]
mod blisp_parser_tests {
    use super::*;
    use crate::blisp_lexer::BLispBrace;

    #[test]
    fn parses_simple_lists() {
        assert_eq!(
            parse(&mut VecDeque::from(vec![BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                                           BLispToken::Expr(BLispExpr::Number(5)),
                                           BLispToken::CloseDelimiter(BLispBrace::Parenthesis)])),
            BLispExpr::cons_sexp(
                BLispExpr::Number(5),
                BLispExpr::Nil
            )
        );

        assert_eq!(
            parse(&mut VecDeque::from(vec![BLispToken::OpenDelimiter(BLispBrace::SquareBrack),
                                           BLispToken::Expr(BLispExpr::Bool(true)),
                                           BLispToken::Expr(BLispExpr::Float(3.82)),
                                           BLispToken::CloseDelimiter(BLispBrace::SquareBrack)])),
            BLispExpr::cons_sexp(
                BLispExpr::Bool(true),
                BLispExpr::cons_sexp(
                    BLispExpr::Float(3.82),
                    BLispExpr::Nil
                )
            )
        );

        assert_eq!(
            parse(&mut VecDeque::from(vec![BLispToken::OpenDelimiter(BLispBrace::CurlyBrack),
                                           BLispToken::Expr(BLispExpr::Symbol("foo".to_string())),
                                           BLispToken::Expr(BLispExpr::Char('e')),
                                           BLispToken::Expr(BLispExpr::Bool(false)),
                                           BLispToken::CloseDelimiter(BLispBrace::CurlyBrack)])),
            BLispExpr::cons_sexp(
                BLispExpr::Symbol("foo".to_string()),
                BLispExpr::cons_sexp(
                    BLispExpr::Char('e'),
                    BLispExpr::cons_sexp(
                        BLispExpr::Bool(false),
                        BLispExpr::Nil
                    )
                )
            )
        );
    }

    #[test]
    fn parses_nested_lists() {
        assert_eq!(
            parse(&mut VecDeque::from(vec![BLispToken::OpenDelimiter(BLispBrace::SquareBrack),
                                           BLispToken::Expr(BLispExpr::Symbol("foo".to_string())),
                                           BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                                           BLispToken::Expr(BLispExpr::Symbol("bar".to_string())),
                                           BLispToken::Expr(BLispExpr::Float(5.34)),
                                           BLispToken::Expr(BLispExpr::Float(3.20)),
                                           BLispToken::CloseDelimiter(BLispBrace::Parenthesis),
                                           BLispToken::Expr(BLispExpr::Number(-3)),
                                           BLispToken::CloseDelimiter(BLispBrace::SquareBrack)])),
            BLispExpr::cons_sexp(
                BLispExpr::Symbol("foo".to_string()),
                BLispExpr::cons_sexp(
                    BLispExpr::cons_sexp(
                        BLispExpr::Symbol("bar".to_string()),
                        BLispExpr::cons_sexp(
                            BLispExpr::Float(5.34),
                            BLispExpr::cons_sexp(
                                BLispExpr::Float(3.20),
                                BLispExpr::Nil
                            )
                        )
                    ),
                    BLispExpr::cons_sexp(
                        BLispExpr::Number(-3),
                        BLispExpr::Nil
                    )
                )
            )
        );
    }

    #[test]
    fn parses_explicit_lists() {
        assert_eq!(
            parse(&mut VecDeque::from(vec![BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                                           BLispToken::Expr(BLispExpr::Number(3)),
                                           BLispToken::SExpDot,
                                           BLispToken::Expr(BLispExpr::Number(4)),
                                           BLispToken::CloseDelimiter(BLispBrace::Parenthesis)])),
            BLispExpr::cons_sexp(
                BLispExpr::Number(3),
                BLispExpr::Number(4)
            )
        );

        assert_eq!(
            parse(&mut VecDeque::from(vec![BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                                           BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                                           BLispToken::Expr(BLispExpr::Number(3)),
                                           BLispToken::SExpDot,
                                           BLispToken::Expr(BLispExpr::Number(4)),
                                           BLispToken::CloseDelimiter(BLispBrace::Parenthesis),
                                           BLispToken::SExpDot,
                                           BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                                           BLispToken::Expr(BLispExpr::Number(5)),
                                           BLispToken::SExpDot,
                                           BLispToken::Expr(BLispExpr::Number(6)),
                                           BLispToken::CloseDelimiter(BLispBrace::Parenthesis),
                                           BLispToken::CloseDelimiter(BLispBrace::Parenthesis)])),
            BLispExpr::cons_sexp(
                BLispExpr::cons_sexp(
                    BLispExpr::Number(3),
                    BLispExpr::Number(4)
                ),
                BLispExpr::cons_sexp(
                    BLispExpr::Number(5),
                    BLispExpr::Number(6)
                ),
            )
        );
    }

    #[test]
    fn parses_lists_with_string_literals() {
        assert_eq!(
            parse(&mut VecDeque::from(vec![BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                                           BLispToken::Expr(BLispExpr::Symbol("foo".to_string())),
                                           BLispToken::StringLiteral(vec!['t','e','s','t']),
                                           BLispToken::CloseDelimiter(BLispBrace::Parenthesis)])),
            BLispExpr::cons_sexp(
                BLispExpr::Symbol("foo".to_string()),
                BLispExpr::cons_sexp(
                    BLispExpr::cons_sexp(
                        BLispExpr::Char('t'),
                        BLispExpr::cons_sexp(
                            BLispExpr::Char('e'),
                            BLispExpr::cons_sexp(
                                BLispExpr::Char('s'),
                                BLispExpr::cons_sexp(
                                    BLispExpr::Char('t'),
                                    BLispExpr::Nil
                                )
                            )
                        )
                    ),
                    BLispExpr::Nil
                )
            )
        );
    }
}
