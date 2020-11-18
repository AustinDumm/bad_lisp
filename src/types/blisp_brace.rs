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
