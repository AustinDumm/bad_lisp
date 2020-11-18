use crate::types::{
    BLispError,
    BLispExpr,
    BLispBrace,
};

pub type BLispTokenResult = Result<BLispToken, BLispError>;

#[derive(Debug, PartialEq, Clone)]
pub struct BLispToken {
    pub token_type: BLispTokenType,
    pub line_num: i32,
    pub col_num: i32,
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
}
