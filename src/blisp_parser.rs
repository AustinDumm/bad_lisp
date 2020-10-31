
use std::collections::VecDeque;

use crate::blisp_expr::{
    BLispExpr,
};

use crate::blisp_lexer::{
    BLispToken,
};

use crate::blisp_func;

pub fn parse(token_queue: &mut VecDeque<BLispToken>) -> BLispExpr {
    match token_queue.front() {
        Some(BLispToken::OpenDelimiter(_)) => parse_list(token_queue),
        Some(BLispToken::QuoteLiteral) => { token_queue.pop_front(); BLispExpr::cons_sexp(BLispExpr::SpecialForm(blisp_func::quote), 
                                                                                          BLispExpr::cons_sexp(parse(token_queue), BLispExpr::Nil)) },
        Some(BLispToken::Expr(_)) => token_queue.pop_front().unwrap().unwrap_expr(),
        Some(BLispToken::StringLiteral(_)) => parse_string_literal(token_queue),
        Some(unexpected) => panic!("Unexpected token found while parsing: {}", unexpected),
        None => panic!("No token found while parsing"),
    }
}

pub fn parse_list(queue: &mut VecDeque<BLispToken>) -> BLispExpr {
    match queue.pop_front() {
        Some(BLispToken::OpenDelimiter(open_brace)) => {
            let contents = parse_list_contents(queue);

            match queue.pop_front() {
                Some(BLispToken::CloseDelimiter(close_brace)) => {
                    if close_brace == open_brace {
                        return contents
                    } else {
                        panic!("Mismatched brace types")
                    }
                },
                _ => panic!("Unexpected closing delimiter to list")
            }
        }
        _ => panic!("Unexpected opening delimiter to list")
    }
}

pub fn parse_list_contents(queue: &mut VecDeque<BLispToken>) -> BLispExpr {
    match queue.front() {
        Some(BLispToken::CloseDelimiter(_)) => BLispExpr::Nil,
        Some(BLispToken::SExpDot) => { queue.pop_front(); parse(queue) },
        _ => BLispExpr::cons_sexp(parse(queue),
                                  parse_list_contents(queue)),
    }
}
    
pub fn parse_string_literal(queue: &mut VecDeque<BLispToken>) -> BLispExpr {
    fn parse_char_iter<'a, I>(mut char_iter: I) -> BLispExpr
    where I: Iterator<Item = &'a char> {
        match char_iter.next() {
            Some(character) => BLispExpr::cons_sexp(BLispExpr::Char(*character),
                                                    parse_char_iter(char_iter)),
            None => BLispExpr::Nil,
        }
    }

    match queue.pop_front() {
        Some(BLispToken::StringLiteral(char_list)) => BLispExpr::cons_sexp(BLispExpr::SpecialForm(crate::blisp_func::quote), 
                                                        BLispExpr::cons_sexp(parse_char_iter(char_list.iter()),
                                                                             BLispExpr::Nil)),
        _ => panic!("Unexpected token found when parsing string literal")
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
