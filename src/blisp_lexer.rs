use crate::blisp_expr::BLispExpr;

use std::iter::Peekable;
use std::convert::TryFrom;
use std::collections::VecDeque;

pub struct BLispCharacterStream<I>
where I: Iterator<Item = char> {
    iterator: Peekable<I>,
    line_num: i32,
    col_num: i32,
}

impl<I> BLispCharacterStream<I> 
where I: Iterator<Item = char> {
    pub fn new(iterator: I) -> BLispCharacterStream<I>
    where I: Iterator<Item = char> {
        BLispCharacterStream { iterator: iterator.peekable(), line_num: 0, col_num: 0 }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.iterator.peek()
    }

    pub fn next(&mut self) -> Option<char> {
        let next = self.iterator.next();
        match next {
            Some('\n') => { self.line_num += 1; self.col_num = 0; }
            _ => self.col_num += 1,
        }

        next
    }

    pub fn current_position(&self) -> (i32, i32) {
        (self.line_num, self.col_num)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BLispToken {
    pub token_type: BLispTokenType,
    line_num: i32,
    col_num: i32,
}

impl BLispToken {
    pub fn new(token_type: BLispTokenType, position: (i32, i32)) -> BLispToken {
        BLispToken { token_type: token_type, line_num: position.0, col_num: position.1 }
    }

    pub fn position(&self) -> (i32, i32) {
        (self.line_num, self.col_num)
    }
}

impl std::fmt::Display for BLispToken {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BLispTokenType {
    Expr(BLispExpr),
    StringLiteral(Vec<char>),
    SExpDot,
    OpenDelimiter(BLispBrace),
    CloseDelimiter(BLispBrace),
    QuoteLiteral,
    QuasiquoteLiteral,
    UnquoteLiteral,
}

impl std::fmt::Display for BLispTokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl BLispTokenType {
    pub fn is_token_interruptor(character: &char) -> bool {
        BLispTokenType::is_open_delimiter(character) ||
        BLispTokenType::is_close_delimiter(character) ||
        character.is_whitespace()
    }

    pub fn is_delimiter(character: &char) -> bool {
        BLispTokenType::is_open_delimiter(character) || 
        BLispTokenType::is_close_delimiter(character)
    }

    pub fn is_open_delimiter(character: &char) -> bool {
        return "({[".contains(|c| c == *character)
    }

    pub fn is_close_delimiter(character: &char) -> bool {
        return ")}]".contains(|c| c == *character)
    }

    pub fn unwrap_expr(self) -> BLispExpr {
        match self {
            BLispTokenType::Expr(expr) => expr,
            _ => panic!("Failed unwrap of BLispTokenType->BLispExpr"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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
    let mut char_iterator = BLispCharacterStream::new(char_iterator);
    let mut token_list: VecDeque<BLispToken> = VecDeque::new();
    while let Some(character) = char_iterator.peek() {
        token_list.push_back (
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
                    BLispToken::new(BLispTokenType::SExpDot, position)
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
            }
        )
    }

    token_list
}

fn lex_delimiter<I>(iterator: &mut BLispCharacterStream<I>) -> BLispToken
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    match iterator.next() {
        Some(character) if BLispTokenType::is_open_delimiter(&character) => BLispToken::new(BLispTokenType::OpenDelimiter(BLispBrace::from(character)), position),
        Some(character) if BLispTokenType::is_close_delimiter(&character) => BLispToken::new(BLispTokenType::CloseDelimiter(BLispBrace::from(character)), position),
        Some(character) => panic!("Unexpected character read as delimiter: {}", character),
        None => panic!("Unexpected end to character stream"),
    }
}

fn lex_backslash<I>(iterator: &mut BLispCharacterStream<I>) -> BLispToken 
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    match iterator.peek() {
        Some('0') => {
            iterator.next();
            BLispToken::new(BLispTokenType::Expr(BLispExpr::Nil), position)
        }
        Some(character) => panic!("Unhandled character: \\{}", character),
        None => panic!("Unexpected end of token stream"),
    }
}

fn lex_octothorpe<I>(iterator: &mut BLispCharacterStream<I>) -> BLispToken
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    match iterator.peek() {
        Some('t') => {
            iterator.next();
            BLispToken::new(BLispTokenType::Expr(BLispExpr::Bool(true)), position)
        }
        Some('f') => {
            iterator.next();
            BLispToken::new(BLispTokenType::Expr(BLispExpr::Bool(false)), position)
        }
        Some('\\') => lex_character_literal(iterator, position),
        Some(character) => panic!("Unhandled character: #{}", character),
        None => panic!("Unexpected end of token stream"),
    }
}

fn lex_character_literal<I>(iterator: &mut BLispCharacterStream<I>, start_position: (i32, i32)) -> BLispToken
where I: Iterator<Item = char> {
    iterator.next();
    match iterator.peek() {
        Some(character) if character.is_digit(10) => 
            lex_character_literal_code(iterator, start_position),
        Some(character) => {
            lex_character_literal_char(*character, iterator, start_position)
        },
        None => panic!("Unexpected end to character stream"),
    }
}

fn lex_character_literal_char<I>(character: char, iterator: &mut BLispCharacterStream<I>, start_position: (i32, i32)) -> BLispToken
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    match iterator.peek() {
        None =>
            BLispToken::new(BLispTokenType::Expr(BLispExpr::Char(character)), start_position),
        Some(next_character) if BLispTokenType::is_token_interruptor(next_character) =>
            BLispToken::new(BLispTokenType::Expr(BLispExpr::Char(character)), start_position),
        Some(next_character) => 
            panic!("Unexpected character after character literal: {}", next_character),
    }
}

fn lex_character_literal_code<I>(iterator: &mut BLispCharacterStream<I>, start_position: (i32, i32)) -> BLispToken
where I: Iterator<Item = char> {
    if let BLispTokenType::Expr(BLispExpr::Number(raw_number)) = lex_digit(false, iterator, start_position).token_type {
        if let Ok(char_code) = u8::try_from(raw_number) {
            BLispToken::new(BLispTokenType::Expr(BLispExpr::Char(char::from(char_code))), start_position)
        } else {
            panic!("Character literal code greater than 255 found");
        }
    } else {
        panic!("Failed to parse character literal code");
    }
}

fn lex_minus<I>(iterator: &mut BLispCharacterStream<I>) -> BLispToken
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    match iterator.peek() {
        Some(character) if character.is_digit(10) || *character == '.' => lex_digit(true, iterator, position),
        None => BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol('-'.to_string())), position),
        Some(character) if BLispTokenType::is_token_interruptor(character) => BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol('-'.to_string())), position),
        Some(character) => panic!("Unexpected character after -: {}", character),
    }
}

fn lex_digit<I>(is_negative: bool, iterator: &mut BLispCharacterStream<I>, start_position: (i32, i32)) -> BLispToken 
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
            panic!("Unexpected character trailing number: {}", peeked_char);
        }
    }


    if is_negative {
        total_number = -total_number;
    }

    BLispToken::new(BLispTokenType::Expr(BLispExpr::Number(total_number)), start_position)
}

fn lex_floating<I>(is_negative: bool, whole_part: i64, iterator: &mut BLispCharacterStream<I>, start_position: (i32, i32)) -> BLispToken
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
            panic!("Unexpected character ending floating point: {}", peeked_char);
        }
    }

    if is_negative {
        total_float = -total_float;
    }
    BLispToken::new(BLispTokenType::Expr(BLispExpr::Float(total_float)), start_position)
}

fn lex_string_literal<I>(iterator: &mut BLispCharacterStream<I>) -> BLispToken
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

    BLispToken::new(BLispTokenType::StringLiteral(string_literal_char_list), position)
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

fn lex_quote_literal<I>(iterator: &mut BLispCharacterStream<I>) -> BLispToken
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    BLispToken::new(BLispTokenType::QuoteLiteral, position)
}

fn lex_quasiquote_literal<I>(iterator: &mut BLispCharacterStream<I>) -> BLispToken
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    BLispToken::new(BLispTokenType::QuasiquoteLiteral, position)
}

fn lex_unquote_literal<I>(iterator: &mut BLispCharacterStream<I>) -> BLispToken
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    iterator.next();
    BLispToken::new(BLispTokenType::UnquoteLiteral, position)
}

fn lex_symbol<I>(iterator: &mut BLispCharacterStream<I>) -> BLispToken
where I: Iterator<Item = char> {
    let position = iterator.current_position();
    let mut symbol = "".to_string();
    while let Some(character) = iterator.peek() {
        match character {
            character if BLispTokenType::is_token_interruptor(character) => break,
            character if BLispExpr::is_disallowed_symbol_char(*character) => panic!("Unexpected character in symbol: {}", character),
            character => {
                symbol.push(*character);
                iterator.next();
            },
        }
    }

    BLispToken::new(BLispTokenType::Expr(BLispExpr::Symbol(symbol)), position)
}

#[cfg(test)]
mod lex_tests {
    use super::*;
    
    #[test]
    fn lexes_single_delimiters() {
        assert_eq!(lex(String::from("(")), vec![BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis)]);
        assert_eq!(lex(String::from("[")), vec![BLispTokenType::OpenDelimiter(BLispBrace::SquareBrack)]);
        assert_eq!(lex(String::from("{")), vec![BLispTokenType::OpenDelimiter(BLispBrace::CurlyBrack)]);

        assert_eq!(lex(String::from(")")), vec![BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis)]);
        assert_eq!(lex(String::from("]")), vec![BLispTokenType::CloseDelimiter(BLispBrace::SquareBrack)]);
        assert_eq!(lex(String::from("}")), vec![BLispTokenType::CloseDelimiter(BLispBrace::CurlyBrack)]);

        assert_eq!(lex(String::from(".")), vec![BLispTokenType::SExpDot]);
    }

    #[test]
    fn lexes_single_exprs() {
        assert_eq!(lex(String::from("\\0")), vec![BLispTokenType::Expr(BLispExpr::Nil)]);

        assert_eq!(lex(String::from("#t")), vec![BLispTokenType::Expr(BLispExpr::Bool(true))]);
        assert_eq!(lex(String::from("#f")), vec![BLispTokenType::Expr(BLispExpr::Bool(false))]);

        assert_eq!(lex(String::from("50")), vec![BLispTokenType::Expr(BLispExpr::Number(50))]);
        assert_eq!(lex(String::from("-50")), vec![BLispTokenType::Expr(BLispExpr::Number(-50))]);

        assert_eq!(lex(String::from("6.29")), vec![BLispTokenType::Expr(BLispExpr::Float(6.29))]);
        assert_eq!(lex(String::from("-6.29")), vec![BLispTokenType::Expr(BLispExpr::Float(-6.29))]);

        assert_eq!(lex(String::from("#\\a")), vec![BLispTokenType::Expr(BLispExpr::Char('a'))]);
        assert_eq!(lex(String::from("#\\34")), vec![BLispTokenType::Expr(BLispExpr::Char('"'))]);

        assert_eq!(lex(String::from("\"testing\"")), vec![BLispTokenType::StringLiteral(vec!['t','e','s','t','i','n','g'])]);
        assert_eq!(lex(String::from("\"te\\nng\"")), vec![BLispTokenType::StringLiteral(vec!['t','e','\n','n','g'])]);

        assert_eq!(lex(String::from("+")), vec![BLispTokenType::Expr(BLispExpr::Symbol(String::from("+")))]);
        assert_eq!(lex(String::from("testing")), vec![BLispTokenType::Expr(BLispExpr::Symbol(String::from("testing")))]);
    }

    #[test]
    fn lexes_compound_exprs() {
        assert_eq!(lex(String::from("(blah 1 2.3)")),
                   vec![BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis),
                        BLispTokenType::Expr(BLispExpr::Symbol(String::from("blah"))),
                        BLispTokenType::Expr(BLispExpr::Number(1)),
                        BLispTokenType::Expr(BLispExpr::Float(2.3)),
                        BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis)]);

        assert_eq!(lex(String::from("[one {two 1 2}]")),
                   vec![BLispTokenType::OpenDelimiter(BLispBrace::SquareBrack),
                        BLispTokenType::Expr(BLispExpr::Symbol(String::from("one"))),
                        BLispTokenType::OpenDelimiter(BLispBrace::CurlyBrack),
                        BLispTokenType::Expr(BLispExpr::Symbol(String::from("two"))),
                        BLispTokenType::Expr(BLispExpr::Number(1)),
                        BLispTokenType::Expr(BLispExpr::Number(2)),
                        BLispTokenType::CloseDelimiter(BLispBrace::CurlyBrack),
                        BLispTokenType::CloseDelimiter(BLispBrace::SquareBrack)]);

        assert_eq!(lex(String::from("(one . (1 . (2 . \\0)))")),
                   vec![BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis),
                        BLispTokenType::Expr(BLispExpr::Symbol(String::from("one"))),
                        BLispTokenType::SExpDot,
                        BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis),
                        BLispTokenType::Expr(BLispExpr::Number(1)),
                        BLispTokenType::SExpDot,
                        BLispTokenType::OpenDelimiter(BLispBrace::Parenthesis),
                        BLispTokenType::Expr(BLispExpr::Number(2)),
                        BLispTokenType::SExpDot,
                        BLispTokenType::Expr(BLispExpr::Nil),
                        BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis),
                        BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis),
                        BLispTokenType::CloseDelimiter(BLispBrace::Parenthesis)]);
    }
}
