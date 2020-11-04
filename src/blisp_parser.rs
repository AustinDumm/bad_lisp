
use std::collections::VecDeque;

use crate::blisp_expr::{
    BLispExpr,
};

use crate::blisp_lexer::{
    BLispToken,
    BLispTokenType,
};

use crate::blisp_func;

pub fn parse(token_queue: &mut VecDeque<BLispToken>) -> BLispExpr {
    match token_queue.front() {
        Some(token) => {
            match token.token_type.clone() {
                BLispTokenType::OpenDelimiter(_) => parse_list(token_queue),
                BLispTokenType::QuoteLiteral => { token_queue.pop_front(); BLispExpr::cons_sexp(BLispExpr::SpecialForm(blisp_func::quote), 
                                                                                                  BLispExpr::cons_sexp(parse(token_queue), BLispExpr::Nil)) },
                BLispTokenType::QuasiquoteLiteral => { token_queue.pop_front(); BLispExpr::cons_sexp(BLispExpr::SpecialForm(blisp_func::quasiquote),
                                                                                                       BLispExpr::cons_sexp(parse(token_queue), BLispExpr::Nil)) },
                BLispTokenType::UnquoteLiteral => { token_queue.pop_front(); BLispExpr::cons_sexp(BLispExpr::Symbol(String::from("unquote")),
                                                                                                    BLispExpr::cons_sexp(parse(token_queue), BLispExpr::Nil)) },
                BLispTokenType::Expr(expr) => { token_queue.pop_front(); expr },
                BLispTokenType::StringLiteral(_) => parse_string_literal(token_queue),
                unexpected => panic!("Unexpected token found while parsing: {}", unexpected),
            }
        }
        None => panic!("No token found while parsing"),
    }
}

pub fn parse_list(queue: &mut VecDeque<BLispToken>) -> BLispExpr {
    match queue.pop_front() {
        Some(token) => {
            match token.token_type {
                BLispTokenType::OpenDelimiter(open_brace) => {
                    let contents = parse_list_contents(queue);

                    match queue.pop_front() {
                        Some(token) => {
                            match token.token_type {
                                BLispTokenType::CloseDelimiter(close_brace) => {
                                    if close_brace == open_brace {
                                        return contents
                                    } else {
                                        panic!("Mismatched brace types")
                                    }
                                },
                                unexpected => panic!("Unexpected end brace to list")
                            }
                        }
                        _ => panic!("Unexpected closing delimiter to list")
                    }
                },
                unexpected => panic!("Unexpected opening delimiter to list: {}", unexpected)
            }
        },
        None => panic!("Unexpected opening delimiter to list")
    }
}

pub fn parse_list_contents(queue: &mut VecDeque<BLispToken>) -> BLispExpr {
    match queue.front() {
        Some(token) => {
            match token.token_type {
                BLispTokenType::CloseDelimiter(_) => BLispExpr::Nil,
                BLispTokenType::SExpDot => { queue.pop_front(); parse(queue) },
                _ => BLispExpr::cons_sexp(parse(queue),
                                          parse_list_contents(queue)),
            }
        }
        None => panic!("No token found while parsing list"),
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
        Some(token) => {
            match token.token_type {
                BLispTokenType::StringLiteral(char_list) => BLispExpr::cons_sexp(BLispExpr::SpecialForm(crate::blisp_func::quote), 
                                                                BLispExpr::cons_sexp(parse_char_iter(char_list.iter()),
                                                                                     BLispExpr::Nil)),
                _ => panic!("Unexpected token found when parsing string literal")
            }
        },
        None => panic!("Unexpected end to token list while parsing string")
    }
}

#[cfg(test)]
mod blisp_parser_tests {
    use super::*;
    use crate::blisp_lexer::BLispBrace;

    #[test]
    fn parses_simple_lists() {
        assert_eq!(
            parse(&mut VecDeque::from(vec![BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(5)), (0, 0)),
                                           BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis), (0, 0))])),
            BLispExpr::cons_sexp(
                BLispExpr::Number(5),
                BLispExpr::Nil
            )
        );

        assert_eq!(
            parse(&mut VecDeque::from(vec![BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::SquareBrack), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Bool(true)), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Float(3.82)), (0, 0)),
                                           BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::SquareBrack), (0, 0))])),
            BLispExpr::cons_sexp(
                BLispExpr::Bool(true),
                BLispExpr::cons_sexp(
                    BLispExpr::Float(3.82),
                    BLispExpr::Nil
                )
            )
        );

        assert_eq!(
            parse(&mut VecDeque::from(vec![BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::CurlyBrack), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol("foo".to_string())), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Char('e')), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Bool(false)), (0, 0)),
                                           BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::CurlyBrack), (0, 0))])),
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
            parse(&mut VecDeque::from(vec![BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::SquareBrack), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol("foo".to_string())), (0, 0)),
                                           BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol("bar".to_string())), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Float(5.34)), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Float(3.20)), (0, 0)),
                                           BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(-3)), (0, 0)),
                                           BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::SquareBrack), (0, 0))])),
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
            parse(&mut VecDeque::from(vec![BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(3)), (0, 0)),
                                           BLispToken::new(BLispTokenType::SExpDot, (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(4)), (0, 0)),
                                           BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis), (0, 0))])),
            BLispExpr::cons_sexp(
                BLispExpr::Number(3),
                BLispExpr::Number(4)
            )
        );

        assert_eq!(
            parse(&mut VecDeque::from(vec![BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis), (0, 0)),
                                           BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(3)), (0, 0)),
                                           BLispToken::new(BLispTokenType::SExpDot, (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(4)), (0, 0)),
                                           BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis), (0, 0)),
                                           BLispToken::new(BLispTokenType::SExpDot, (0, 0)),
                                           BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(5)), (0, 0)),
                                           BLispToken::new(BLispTokenType::SExpDot, (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(6)), (0, 0)),
                                           BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis), (0, 0)),
                                           BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis), (0, 0))])),
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
            parse(&mut VecDeque::from(vec![BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis), (0, 0)),
                                           BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol("foo".to_string())), (0, 0)),
                                           BLispToken::new(BLispTokenType::StringLiteral(vec!['t','e','s','t']), (0, 0)),
                                           BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis), (0, 0))])),
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
