#[derive(Debug, PartialEq, Clone)]
pub enum BLispErrorType {
    Lexing,
    Parsing,
    Evaluation,
}

impl std::fmt::Display for BLispErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BLispErrorType::Lexing => write!(f, "Lexing Error"),
            BLispErrorType::Parsing => write!(f, "Parsing Error"),
            BLispErrorType::Evaluation => write!(f, "Evaluation Error"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BLispError {
    error_type: BLispErrorType,
    message: String,
    position: Option<(i32, i32)>,
}

impl BLispError {
    pub fn new(error_type: BLispErrorType, message: String, position: Option<(i32, i32)>) -> BLispError {
        BLispError { error_type: error_type, message: message, position: position }
    }
}

impl std::fmt::Display for BLispError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.position {
            Some(position) => write!(f, "{} | {:<70} | {}:{}", self.error_type, self.message, position.0, position.1),
            None => write!(f, "{} | {:<70}", self.error_type, self.message),
        }
    }
}
