
use std::collections::VecDeque;

mod blisp_lexer;
mod blisp_expr;
mod blisp_parser;

fn main() {
    println!("Hello, world!");
    blisp_lexer::lex(String::from("(add 1 2)"));
    blisp_parser::parse(&mut VecDeque::new());
}
