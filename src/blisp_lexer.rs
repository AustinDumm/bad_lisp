use crate::blisp_expr::BLispExpr;

use std::iter::Peekable;
use std::convert::TryFrom;
use std::collections::VecDeque;

#[derive(Debug, PartialEq)]
pub enum BLispToken {
    Expr(BLispExpr),
    StringLiteral(Vec<char>),
    SExpDot,
    OpenDelimiter(BLispBrace),
    CloseDelimiter(BLispBrace),
}

impl BLispToken {
    pub fn is_token_interruptor(character: &char) -> bool {
        BLispToken::is_open_delimiter(character) ||
        BLispToken::is_close_delimiter(character) ||
        character.is_whitespace()
    }

    pub fn is_delimiter(character: &char) -> bool {
        BLispToken::is_open_delimiter(character) || 
        BLispToken::is_close_delimiter(character)
    }

    pub fn is_open_delimiter(character: &char) -> bool {
        return "({[".contains(|c| c == *character)
    }

    pub fn is_close_delimiter(character: &char) -> bool {
        return ")}]".contains(|c| c == *character)
    }
}

#[derive(Debug, PartialEq)]
pub enum BLispBrace {
    Parenthesis,
    SquareBrack,
    CurlyBrack,
}

impl BLispBrace {
    pub fn from(character: char) -> BLispBrace {
        match character {
            '(' | ')' => BLispBrace::Parenthesis,
            '[' | ']' => BLispBrace::SquareBrack,
            '{' | '}' => BLispBrace::CurlyBrack,
            character => panic!("Unexpected brace character: {}", character),
        }
    }
}


pub fn lex(raw_string: String) -> VecDeque<BLispToken> {
    let char_iterator = raw_string.chars();
    let mut char_iterator = char_iterator.peekable();
    let mut token_list: VecDeque<BLispToken> = VecDeque::new();
    println!("parsing: {}", raw_string);
    while let Some(character) = char_iterator.peek() {
        token_list.push_back (
            match character {
                character if character.is_whitespace() => {
                    char_iterator.next();
                    continue;
                },
                character if BLispToken::is_delimiter(character) => {
                    lex_delimiter(&mut char_iterator)
                },
                '.' => {
                    char_iterator.next();
                    BLispToken::SExpDot
                },
                '\\' => {
                    lex_backslash(&mut char_iterator)
                },
                '#' => {
                    lex_octothorpe(&mut char_iterator)
                },
                character if character.is_digit(10) => {
                    lex_digit(false, &mut char_iterator)
                },
                '-' => {
                    lex_minus(&mut char_iterator)
                },
                '"' => {
                    lex_string_literal(&mut char_iterator)
                },
                _ => {
                    lex_symbol(&mut char_iterator)
                },
            }
        )
    }
    println!("done with: {}", raw_string);

    token_list
}

fn lex_delimiter<I>(iterator: &mut Peekable<I>) -> BLispToken
where I: Iterator<Item = char> {
    match iterator.next() {
        Some(character) if BLispToken::is_open_delimiter(&character) => BLispToken::OpenDelimiter(BLispBrace::from(character)),
        Some(character) if BLispToken::is_close_delimiter(&character) => BLispToken::CloseDelimiter(BLispBrace::from(character)),
        Some(character) => panic!("Unexpected character read as delimiter: {}", character),
        None => panic!("Unexpected end to character stream"),
    }
}

fn lex_backslash<I>(iterator: &mut Peekable<I>) -> BLispToken 
where I: Iterator<Item = char> {
    iterator.next();
    match iterator.peek() {
        Some('0') => {
            iterator.next();
            BLispToken::Expr(BLispExpr::Nil)
        }
        Some(character) => panic!("Unhandled character: \\{}", character),
        None => panic!("Unexpected end of token stream"),
    }
}

fn lex_octothorpe<I>(iterator: &mut Peekable<I>) -> BLispToken
where I: Iterator<Item = char> {
    iterator.next();
    match iterator.peek() {
        Some('t') => {
            iterator.next();
            BLispToken::Expr(BLispExpr::Bool(true))
        }
        Some('f') => {
            iterator.next();
            BLispToken::Expr(BLispExpr::Bool(false))
        }
        Some('\\') => lex_character_literal(iterator),
        Some(character) => panic!("Unhandled character: #{}", character),
        None => panic!("Unexpected end of token stream"),
    }
}

fn lex_character_literal<I>(iterator: &mut Peekable<I>) -> BLispToken
where I: Iterator<Item = char> {
    iterator.next();
    match iterator.peek() {
        Some(character) if character.is_digit(10) => 
            lex_character_literal_code(iterator),
        Some(character) => {
            lex_character_literal_char(*character, iterator)
        },
        None => panic!("Unexpected end to character stream"),
    }
}

fn lex_character_literal_char<I>(character: char, iterator: &mut Peekable<I>) -> BLispToken
where I: Iterator<Item = char> {
    iterator.next();
    match iterator.peek() {
        None =>
            BLispToken::Expr(BLispExpr::Char(character)),
        Some(next_character) if BLispToken::is_token_interruptor(next_character) =>
            BLispToken::Expr(BLispExpr::Char(character)),
        Some(next_character) => 
            panic!("Unexpected character after character literal: {}", next_character),
    }
}

fn lex_character_literal_code<I>(iterator: &mut Peekable<I>) -> BLispToken
where I: Iterator<Item = char> {
    if let BLispToken::Expr(BLispExpr::Number(raw_number)) = lex_digit(false, iterator) {
        if let Ok(char_code) = u8::try_from(raw_number) {
            BLispToken::Expr(BLispExpr::Char(char::from(char_code)))
        } else {
            panic!("Character literal code greater than 255 found");
        }
    } else {
        panic!("Failed to parse character literal code");
    }
}

fn lex_minus<I>(iterator: &mut Peekable<I>) -> BLispToken
where I: Iterator<Item = char> {
    iterator.next();
    match iterator.peek() {
        Some(character) if character.is_digit(10) || *character == '.' => lex_digit(true, iterator),
        None => BLispToken::Expr(BLispExpr::Symbol('-'.to_string())),
        Some(character) if BLispToken::is_token_interruptor(character) => BLispToken::Expr(BLispExpr::Symbol('-'.to_string())),
        Some(character) => panic!("Unexpected character after -: {}", character),
    }
}

fn lex_digit<I>(is_negative: bool, iterator: &mut Peekable<I>) -> BLispToken 
where I: Iterator<Item = char> {
    let mut total_number: i64 = 0;
    while let Some(peeked_char) = iterator.peek() {
        if let Some(next_digit) = peeked_char.to_digit(10) {
            iterator.next();
            total_number = total_number * 10 + i64::from(next_digit);
        } else if *peeked_char == '.' {
            return lex_floating(is_negative, total_number, iterator);
        } else if BLispToken::is_token_interruptor(peeked_char) {
            break;
        } else {
            panic!("Unexpected character trailing number: {}", peeked_char);
        }
    }


    if is_negative {
        total_number = -total_number;
    }

    BLispToken::Expr(BLispExpr::Number(total_number))
}

fn lex_floating<I>(is_negative: bool, whole_part: i64, iterator: &mut Peekable<I>) -> BLispToken
where I: Iterator<Item = char> {
    let mut total_float: f64 = whole_part as f64;
    let mut divisor: f64 = 10.0;
    iterator.next();

    while let Some(peeked_char) = iterator.peek() {
        if let Some(next_digit) = peeked_char.to_digit(10) {
            iterator.next();
            total_float = total_float + next_digit as f64 / divisor;
            divisor = divisor * 10.0;
        } else if BLispToken::is_token_interruptor(peeked_char) {
            break;
        } else {
            panic!("Unexpected character ending floating point: {}", peeked_char);
        }
    }

    if is_negative {
        total_float = -total_float;
    }
    BLispToken::Expr(BLispExpr::Float(total_float))
}

fn lex_string_literal<I>(iterator: &mut Peekable<I>) -> BLispToken
where I: Iterator<Item = char> {
    iterator.next();
    let mut string_literal_char_list: Vec<char> = Vec::new();
    while let Some(character) = iterator.next() {
        match character {
            '"' => break,
            '\\' => string_literal_char_list.push(lex_string_literal_escape_sequence(iterator)),
            character => string_literal_char_list.push(character),
        }
    }

    BLispToken::StringLiteral(string_literal_char_list)
}

fn lex_string_literal_escape_sequence<I>(iterator: &mut Peekable<I>) -> char
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

fn lex_symbol<I>(iterator: &mut Peekable<I>) -> BLispToken
where I: Iterator<Item = char> {
    let mut symbol = "".to_string();
    while let Some(character) = iterator.peek() {
        match character {
            character if BLispExpr::is_disallowed_symbol_char(*character) => panic!("Unexpected character in symbol: {}", character),
            character if BLispToken::is_token_interruptor(character) => break,
            character => {
                symbol.push(*character);
                iterator.next();
            },
        }
    }

    BLispToken::Expr(BLispExpr::Symbol(symbol))
}

#[cfg(test)]
mod lex_tests {
    use super::*;
    
    #[test]
    fn lexes_single_delimiters() {
        assert_eq!(lex(String::from("(")), vec![BLispToken::OpenDelimiter(BLispBrace::Parenthesis)]);
        assert_eq!(lex(String::from("[")), vec![BLispToken::OpenDelimiter(BLispBrace::SquareBrack)]);
        assert_eq!(lex(String::from("{")), vec![BLispToken::OpenDelimiter(BLispBrace::CurlyBrack)]);

        assert_eq!(lex(String::from(")")), vec![BLispToken::CloseDelimiter(BLispBrace::Parenthesis)]);
        assert_eq!(lex(String::from("]")), vec![BLispToken::CloseDelimiter(BLispBrace::SquareBrack)]);
        assert_eq!(lex(String::from("}")), vec![BLispToken::CloseDelimiter(BLispBrace::CurlyBrack)]);

        assert_eq!(lex(String::from(".")), vec![BLispToken::SExpDot]);
    }

    #[test]
    fn lexes_single_exprs() {
        assert_eq!(lex(String::from("\\0")), vec![BLispToken::Expr(BLispExpr::Nil)]);

        assert_eq!(lex(String::from("#t")), vec![BLispToken::Expr(BLispExpr::Bool(true))]);
        assert_eq!(lex(String::from("#f")), vec![BLispToken::Expr(BLispExpr::Bool(false))]);

        assert_eq!(lex(String::from("50")), vec![BLispToken::Expr(BLispExpr::Number(50))]);
        assert_eq!(lex(String::from("-50")), vec![BLispToken::Expr(BLispExpr::Number(-50))]);

        assert_eq!(lex(String::from("6.29")), vec![BLispToken::Expr(BLispExpr::Float(6.29))]);
        assert_eq!(lex(String::from("-6.29")), vec![BLispToken::Expr(BLispExpr::Float(-6.29))]);

        assert_eq!(lex(String::from("#\\a")), vec![BLispToken::Expr(BLispExpr::Char('a'))]);
        assert_eq!(lex(String::from("#\\34")), vec![BLispToken::Expr(BLispExpr::Char('"'))]);

        assert_eq!(lex(String::from("\"testing\"")), vec![BLispToken::StringLiteral(vec!['t','e','s','t','i','n','g'])]);
        assert_eq!(lex(String::from("\"te\\nng\"")), vec![BLispToken::StringLiteral(vec!['t','e','\n','n','g'])]);

        assert_eq!(lex(String::from("+")), vec![BLispToken::Expr(BLispExpr::Symbol(String::from("+")))]);
        assert_eq!(lex(String::from("testing")), vec![BLispToken::Expr(BLispExpr::Symbol(String::from("testing")))]);
    }

    #[test]
    fn lexes_compound_exprs() {
        assert_eq!(lex(String::from("(blah 1 2.3)")),
                   vec![BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                        BLispToken::Expr(BLispExpr::Symbol(String::from("blah"))),
                        BLispToken::Expr(BLispExpr::Number(1)),
                        BLispToken::Expr(BLispExpr::Float(2.3)),
                        BLispToken::CloseDelimiter(BLispBrace::Parenthesis)]);

        assert_eq!(lex(String::from("[one {two 1 2}]")),
                   vec![BLispToken::OpenDelimiter(BLispBrace::SquareBrack),
                        BLispToken::Expr(BLispExpr::Symbol(String::from("one"))),
                        BLispToken::OpenDelimiter(BLispBrace::CurlyBrack),
                        BLispToken::Expr(BLispExpr::Symbol(String::from("two"))),
                        BLispToken::Expr(BLispExpr::Number(1)),
                        BLispToken::Expr(BLispExpr::Number(2)),
                        BLispToken::CloseDelimiter(BLispBrace::CurlyBrack),
                        BLispToken::CloseDelimiter(BLispBrace::SquareBrack)]);

        assert_eq!(lex(String::from("(one . (1 . (2 . \\0)))")),
                   vec![BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                        BLispToken::Expr(BLispExpr::Symbol(String::from("one"))),
                        BLispToken::SExpDot,
                        BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                        BLispToken::Expr(BLispExpr::Number(1)),
                        BLispToken::SExpDot,
                        BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                        BLispToken::Expr(BLispExpr::Number(2)),
                        BLispToken::SExpDot,
                        BLispToken::Expr(BLispExpr::Nil),
                        BLispToken::CloseDelimiter(BLispBrace::Parenthesis),
                        BLispToken::CloseDelimiter(BLispBrace::Parenthesis),
                        BLispToken::CloseDelimiter(BLispBrace::Parenthesis)]);
    }
}
