
mod blisp_lexer;
mod blisp_expr;

fn main() {
    println!("Hello, world!");
    blisp_lexer::lex(String::from("(add 1 2)"));
}
