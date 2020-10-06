use crate::blisp_expr::BLispExpr;

#[derive(Debug, PartialEq)]
pub enum BLispToken {
    Expr(BLispExpr),
    StringLiteral(String),
    SExpDelimiter,
    OpenDelimiter(BLispBrace),
    CloseDelimiter(BLispBrace),
}

#[derive(Debug, PartialEq)]
enum BLispBrace {
    Parenthesis,
    SquareBrack,
    CurlyBrack,
}

pub fn lex(raw_string: &str) -> Vec<BLispToken> {

    Vec::new()
}

#[cfg(test)]
mod LexTests {
    use super::*;
    
    #[test]
    fn lexes_single_delimiters() {
        assert_eq!(lex("("), vec![BLispToken::OpenDelimiter(BLispBrace::Parenthesis)]);
        assert_eq!(lex("["), vec![BLispToken::OpenDelimiter(BLispBrace::SquareBrack)]);
        assert_eq!(lex("{"), vec![BLispToken::OpenDelimiter(BLispBrace::CurlyBrack)]);

        assert_eq!(lex(")"), vec![BLispToken::CloseDelimiter(BLispBrace::Parenthesis)]);
        assert_eq!(lex("]"), vec![BLispToken::CloseDelimiter(BLispBrace::SquareBrack)]);
        assert_eq!(lex("}"), vec![BLispToken::CloseDelimiter(BLispBrace::CurlyBrack)]);
    }

    #[test]
    fn lexes_single_exprs() {
        assert_eq!(lex("\\0"), vec![BLispToken::Expr(BLispExpr::Nil)]);
        assert_eq!(lex("."), vec![BLispToken::SExpDelimiter]);

        assert_eq!(lex("#t"), vec![BLispToken::Expr(BLispExpr::Bool(true))]);
        assert_eq!(lex("#f"), vec![BLispToken::Expr(BLispExpr::Bool(false))]);

        assert_eq!(lex("50"), vec![BLispToken::Expr(BLispExpr::Number(50))]);
        assert_eq!(lex("-50"), vec![BLispToken::Expr(BLispExpr::Number(-50))]);

        assert_eq!(lex("6.29"), vec![BLispToken::Expr(BLispExpr::Float(6.29))]);
        assert_eq!(lex("-6.29"), vec![BLispToken::Expr(BLispExpr::Float(-6.29))]);

        assert_eq!(lex("#\\a)"), vec![BLispToken::Expr(BLispExpr::Char('a'))]);
        assert_eq!(lex("#\\space"), vec![BLispToken::Expr(BLispExpr::Char(' '))]);
        assert_eq!(lex("#\\34"), vec![BLispToken::Expr(BLispExpr::Char('"'))]);

        assert_eq!(lex("\"testing\""), vec![BLispToken::StringLiteral(String::from("testing"))]);
        assert_eq!(lex("\"te\\nng\""), vec![BLispToken::StringLiteral(String::from("te\\nng"))]);

        assert_eq!(lex("testing"), vec![BLispToken::Expr(BLispExpr::Symbol(String::from("testing")))]);
        assert_eq!(lex("te\\nng"), vec![BLispToken::Expr(BLispExpr::Symbol(String::from("te\\nng")))]);
    }

    #[test]
    fn lexes_compound_exprs() {
        assert_eq!(lex("(blah 1 2.3)"),
                   vec![BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                        BLispToken::Expr(BLispExpr::Symbol(String::from("blah"))),
                        BLispToken::Expr(BLispExpr::Number(1)),
                        BLispToken::Expr(BLispExpr::Float(2.3)),
                        BLispToken::CloseDelimiter(BLispBrace::Parenthesis)]);

        assert_eq!(lex("[one {two 1 2}]"),
                   vec![BLispToken::OpenDelimiter(BLispBrace::SquareBrack),
                        BLispToken::Expr(BLispExpr::Symbol(String::from("one"))),
                        BLispToken::OpenDelimiter(BLispBrace::CurlyBrack),
                        BLispToken::Expr(BLispExpr::Symbol(String::from("two"))),
                        BLispToken::Expr(BLispExpr::Number(1)),
                        BLispToken::Expr(BLispExpr::Number(2)),
                        BLispToken::CloseDelimiter(BLispBrace::CurlyBrack),
                        BLispToken::CloseDelimiter(BLispBrace::SquareBrack)]);

        assert_eq!(lex("(one . (1 . (2 . \\0)))"),
                   vec![BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                        BLispToken::Expr(BLispExpr::Symbol(String::from("one"))),
                        BLispToken::SExpDelimiter,
                        BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                        BLispToken::Expr(BLispExpr::Number(1)),
                        BLispToken::SExpDelimiter,
                        BLispToken::OpenDelimiter(BLispBrace::Parenthesis),
                        BLispToken::Expr(BLispExpr::Number(2)),
                        BLispToken::SExpDelimiter,
                        BLispToken::Expr(BLispExpr::Nil),
                        BLispToken::CloseDelimiter(BLispBrace::Parenthesis),
                        BLispToken::CloseDelimiter(BLispBrace::Parenthesis),
                        BLispToken::CloseDelimiter(BLispBrace::Parenthesis)]);
    }
}
