use std::convert::TryFrom;
use std::collections::VecDeque;

use crate::types::{
    BLispExpr,
    BLispCharacterStream,
    BLispError,
    BLispErrorType,
    BLispToken,
    BLispTokenType,
    BLispTokenResult,
    BLispBrace,
};

pub fn lex(raw_string: String) -> Result<VecDeque<BLispToken>, String> {
    let char_iterator = raw_string.chars();
    let mut char_iterator = BLispCharacterStream::new(char_iterator);
    let mut token_list: VecDeque<BLispToken> = VecDeque::new();
    while let Some(character) = char_iterator.peek() {
        let token_result: BLispTokenResult = 
            match character {
                character if character.is_whitespace() => {
                    char_iterator.next();
                    continue;
                },
                character if BLispTokenType::is_delimiter(character) => {
                    lex_delimiter(&mut char_iterator)
                },
                '.' => {
                    let position = char_iterator.current_position();
                    char_iterator.next();
                    Ok(BLispToken::new(BLispTokenType::SExpDot, position))
                },
                '\\' => {
                    lex_backslash(&mut char_iterator)
                },
                '#' => {
                    lex_octothorpe(&mut char_iterator)
                },
                character if character.is_digit(10) => {
                    let position = char_iterator.current_position();
                    lex_digit(false, &mut char_iterator, position)
                },
                '-' => {
                    lex_minus(&mut char_iterator)
                },
                '"' => {
                    lex_string_literal(&mut char_iterator)
                },
                '\'' => {
                    lex_quote_literal(&mut char_iterator)
                },
                '`' => {
                    lex_quasiquote_literal(&mut char_iterator)
                },
                ',' => {
                    lex_unquote_literal(&mut char_iterator)
                }
                _ => {
                    lex_symbol(&mut char_iterator)
                },
            };
        
        match token_result {
            Ok(token) => token_list.push_back(token),
            Err(error) => return Err(format!("{}", error))
        }
    }

    Ok(token_list)
}

fn lex_delimiter<I>(iterator: &mut BLispCharacterStream<I>) -> BLispTokenResult
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    match iterator.next() {
        Some(character) if BLispTokenType::is_open_delimiter(&character) => Ok(BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::from(character)), position)),
        Some(character) if BLispTokenType::is_close_delimiter(&character) => Ok(BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::from(character)), position)),
        Some(character) => Err(BLispError::new(BLispErrorType::Lexing, format!("Unexpected character read as delimiter: {}", character), Some(position))),
        None => Err(BLispError::new(BLispErrorType::Lexing, format!("Unexpected end to character stream while lexing delimiter"), Some(position))),
    }
}

fn lex_backslash<I>(iterator: &mut BLispCharacterStream<I>) -> BLispTokenResult 
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    match iterator.peek() {
        Some('0') => {
            iterator.next();
            Ok(BLispToken::new(BLispTokenType::Expr(BLispExpr::Nil), position))
        }
        Some(character) => Err(BLispError::new(BLispErrorType::Lexing, format!("Unhandled character: \\{}", character), Some(position))),
        None => Err(BLispError::new(BLispErrorType::Lexing, format!("Unexpected end of token stream after backslash"), Some(position))),
    }
}

fn lex_octothorpe<I>(iterator: &mut BLispCharacterStream<I>) -> BLispTokenResult
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    match iterator.peek() {
        Some('t') => {
            iterator.next();
            Ok(BLispToken::new(BLispTokenType::Expr(BLispExpr::Bool(true)), position))
        }
        Some('f') => {
            iterator.next();
            Ok(BLispToken::new(BLispTokenType::Expr(BLispExpr::Bool(false)), position))
        }
        Some('\\') => lex_character_literal(iterator, position),
        Some(character) => Err(BLispError::new(BLispErrorType::Lexing, format!("Unhandled character after octothorpe: {}", character), Some(position))),
        None => Err(BLispError::new(BLispErrorType::Lexing, format!("Unexpected end of token stream after octothorpe"), Some(position))),
    }
}

fn lex_character_literal<I>(iterator: &mut BLispCharacterStream<I>, start_position: (i32, i32)) -> BLispTokenResult
where I: Iterator<Item = char> {
    iterator.next();
    match iterator.peek() {
        Some(character) if character.is_digit(10) => 
            lex_character_literal_code(iterator, start_position),
        Some(character) => {
            lex_character_literal_char(*character, iterator, start_position)
        },
        None => Err(BLispError::new(BLispErrorType::Lexing, format!("Unexpected end to character stream in character literal"), Some(start_position))),
    }
}

fn lex_character_literal_char<I>(character: char, iterator: &mut BLispCharacterStream<I>, start_position: (i32, i32)) -> BLispTokenResult
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    match iterator.peek() {
        None =>
            Ok(BLispToken::new(BLispTokenType::Expr(BLispExpr::Char(character)), start_position)),
        Some(next_character) if BLispTokenType::is_token_interruptor(next_character) =>
            Ok(BLispToken::new(BLispTokenType::Expr(BLispExpr::Char(character)), start_position)),
        Some(next_character) => 
            Err(BLispError::new(BLispErrorType::Lexing, format!("Unexpected character after character literal: {}", next_character), Some(position))),
    }
}

fn lex_character_literal_code<I>(iterator: &mut BLispCharacterStream<I>, start_position: (i32, i32)) -> BLispTokenResult
where I: Iterator<Item = char> {
    match lex_digit(false, iterator, start_position) {
        Ok(token) => {
            match token.token_type {
                BLispTokenType::Expr(BLispExpr::Number(raw_number)) => {
                    if let Ok(char_code) = u8::try_from(raw_number) {
                        Ok(BLispToken::new(BLispTokenType::Expr(BLispExpr::Char(char::from(char_code))), start_position))
                    } else {
                        Err(BLispError::new(BLispErrorType::Lexing, format!("Character literal code greater than 255 found"), Some(start_position)))
                    }
                },
                unexpected => Err(BLispError::new(BLispErrorType::Lexing, format!("Unexpected character found while lexing character literal code: {}", unexpected), Some(start_position))),
            }
        },
        error => {
            error
        },
    }
}

fn lex_minus<I>(iterator: &mut BLispCharacterStream<I>) -> BLispTokenResult
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    match iterator.peek() {
        Some(character) if character.is_digit(10) || *character == '.' => lex_digit(true, iterator, position),
        None => Ok(BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol('-'.to_string())), position)),
        Some(character) if BLispTokenType::is_token_interruptor(character) => Ok(BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol('-'.to_string())), position)),
        Some(_) => 
            match lex_symbol(iterator) {
                Ok(token) =>
                    match token.clone().token_type {
                        BLispTokenType::Expr(BLispExpr::Symbol(sym)) => Ok(BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol(format!("-{}", sym))), token.position())),
                        expr => Err(BLispError::new(BLispErrorType::Lexing, format!("Unexpected expression found following character '-': {}", expr), Some(token.position()))),
                    }
                Err(error) => Err(error),
            }
    }
}

fn lex_digit<I>(is_negative: bool, iterator: &mut BLispCharacterStream<I>, start_position: (i32, i32)) -> BLispTokenResult
where I: Iterator<Item = char> {
    let mut total_number: i64 = 0;
    while let Some(peeked_char) = iterator.peek() {
        if let Some(next_digit) = peeked_char.to_digit(10) {
            iterator.next();
            total_number = total_number * 10 + i64::from(next_digit);
        } else if *peeked_char == '.' {
            return lex_floating(is_negative, total_number, iterator, start_position);
        } else if BLispTokenType::is_token_interruptor(peeked_char) {
            break;
        } else {
            return Err(BLispError::new(BLispErrorType::Lexing, format!("Unexpected character trailing number: {}", peeked_char), Some(start_position)))
        }
    }


    if is_negative {
        total_number = -total_number;
    }

    Ok(BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(total_number)), start_position))
}

fn lex_floating<I>(is_negative: bool, whole_part: i64, iterator: &mut BLispCharacterStream<I>, start_position: (i32, i32)) -> BLispTokenResult
where I: Iterator<Item = char> {
    let mut total_float: f64 = whole_part as f64;
    let mut divisor: f64 = 10.0;
    iterator.next();

    while let Some(peeked_char) = iterator.peek() {
        if let Some(next_digit) = peeked_char.to_digit(10) {
            iterator.next();
            total_float = total_float + next_digit as f64 / divisor;
            divisor = divisor * 10.0;
        } else if BLispTokenType::is_token_interruptor(peeked_char) {
            break;
        } else {
            return Err(BLispError::new(BLispErrorType::Lexing, format!("Unexpected character ending floating point: {}", peeked_char), Some(start_position)))
        }
    }

    if is_negative {
        total_float = -total_float;
    }
    Ok(BLispToken::new(BLispTokenType::Expr(BLispExpr::Float(total_float)), start_position))
}

fn lex_string_literal<I>(iterator: &mut BLispCharacterStream<I>) -> BLispTokenResult
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    let mut string_literal_char_list: Vec<char> = Vec::new();
    while let Some(character) = iterator.next() {
        match character {
            '"' => break,
            '\\' => string_literal_char_list.push(lex_string_literal_escape_sequence(iterator)),
            character => string_literal_char_list.push(character),
        }
    }

    Ok(BLispToken::new(BLispTokenType::StringLiteral(string_literal_char_list), position))
}

fn lex_string_literal_escape_sequence<I>(iterator: &mut BLispCharacterStream<I>) -> char
where I: Iterator<Item = char> {
    match iterator.next() {
        Some('r') => '\r',
        Some('n') => '\n',
        Some('t') => '\t',
        Some('\\') => '\\',
        Some('"') => '"',
        Some(character) => panic!("Unexpected escape sequence in string literal: \\{}", character),
        None => panic!("Unexpected end to character stream in escape sequence"),
    }
}

fn lex_quote_literal<I>(iterator: &mut BLispCharacterStream<I>) -> BLispTokenResult
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    Ok(BLispToken::new(BLispTokenType::QuoteLiteral, position))
}

fn lex_quasiquote_literal<I>(iterator: &mut BLispCharacterStream<I>) -> BLispTokenResult
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    Ok(BLispToken::new(BLispTokenType::QuasiquoteLiteral, position))
}

fn lex_unquote_literal<I>(iterator: &mut BLispCharacterStream<I>) -> BLispTokenResult
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    Ok(BLispToken::new(BLispTokenType::UnquoteLiteral, position))
}

fn lex_symbol<I>(iterator: &mut BLispCharacterStream<I>) -> BLispTokenResult
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    let mut symbol = "".to_string();
    while let Some(character) = iterator.peek() {
        match character {
            character if BLispTokenType::is_token_interruptor(character) => break,
            character if BLispExpr::is_disallowed_symbol_char(*character) => return Err(BLispError::new(BLispErrorType::Lexing, format!("Unexpected character in symbol: {}", character), Some(position))),
            character => {
                symbol.push(*character);
                iterator.next();
            },
        }
    }

    Ok(BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol(symbol)), position))
}

#[cfg(test)]
mod lex_tests {
    use super::*;
    use std::collections::VecDeque;
    
    #[test]
    fn lexes_single_delimiters() {
        assert_eq!(lex(String::from("(")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis), (1, 1))])));
        assert_eq!(lex(String::from("[")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::SquareBrack), (1, 1))])));
        assert_eq!(lex(String::from("{")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::CurlyBrack), (1, 1))])));

        assert_eq!(lex(String::from(")")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis), (1, 1))])));
        assert_eq!(lex(String::from("]")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::SquareBrack), (1, 1))])));
        assert_eq!(lex(String::from("}")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::CurlyBrack), (1, 1))])));

        assert_eq!(lex(String::from(".")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::SExpDot, (1, 1))])));
    }

    #[test]
    fn lexes_single_exprs() {
        assert_eq!(lex(String::from("\\0")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::Expr(BLispExpr::Nil), (1, 1))])));

        assert_eq!(lex(String::from("#t")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::Expr(BLispExpr::Bool(true)), (1, 1))])));
        assert_eq!(lex(String::from("#f")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::Expr(BLispExpr::Bool(false)), (1, 1))])));

        assert_eq!(lex(String::from("50")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(50)), (1, 1))])));
        assert_eq!(lex(String::from("-50")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(-50)), (1, 1))])));

        assert_eq!(lex(String::from("6.29")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::Expr(BLispExpr::Float(6.29)), (1, 1))])));
        assert_eq!(lex(String::from("-6.29")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::Expr(BLispExpr::Float(-6.29)), (1, 1))])));

        assert_eq!(lex(String::from("#\\a")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::Expr(BLispExpr::Char('a')), (1, 1))])));
        assert_eq!(lex(String::from("#\\34")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::Expr(BLispExpr::Char('"')), (1, 1))])));

        assert_eq!(lex(String::from("\"testing\"")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::StringLiteral(vec!['t','e','s','t','i','n','g']), (1, 1))])));
        assert_eq!(lex(String::from("\"te\\nng\"")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::StringLiteral(vec!['t','e','\n','n','g']), (1, 1))])));

        assert_eq!(lex(String::from("+")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol(String::from("+"))), (1, 1))])));
        assert_eq!(lex(String::from("testing")), Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol(String::from("testing"))), (1, 1))])));
    }

    #[test]
    fn lexes_compound_exprs() {
        assert_eq!(lex(String::from("(blah 1 2.3)")),
                   Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis), (1, 1)),
                                         BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol(String::from("blah"))), (1, 2)),
                                         BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(1)), (1, 7)),
                                         BLispToken::new(BLispTokenType::Expr(BLispExpr::Float(2.3)), (1, 9)),
                                         BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis), (1, 12))])));

        assert_eq!(lex(String::from("[one {two 1 2}]")),
                   Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::SquareBrack), (1, 1)),
                                          BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol(String::from("one"))), (1, 2)),
                                          BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::CurlyBrack), (1, 6)),
                                          BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol(String::from("two"))), (1, 7)),
                                          BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(1)), (1, 11)),
                                          BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(2)), (1, 13)),
                                          BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::CurlyBrack), (1, 14)),
                                          BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::SquareBrack), (1, 15))])));

        assert_eq!(lex(String::from("(one . (1 . (2 . \\0)))")),
                   Ok(VecDeque::from(vec![BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis), (1, 1)),
                                          BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol(String::from("one"))), (1, 2)),
                                          BLispToken::new(BLispTokenType::SExpDot, (1, 6)),
                                          BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis), (1, 8)),
                                          BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(1)), (1, 9)),
                                          BLispToken::new(BLispTokenType::SExpDot, (1, 11)),
                                          BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis), (1, 13)),
                                          BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(2)), (1, 14)),
                                          BLispToken::new(BLispTokenType::SExpDot, (1, 16)),
                                          BLispToken::new(BLispTokenType::Expr(BLispExpr::Nil), (1, 18)),
                                          BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis), (1, 20)),
                                          BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis), (1, 21)),
                                          BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis), (1, 22))])));
    }
}
