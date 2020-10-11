
mod blisp_lexer;
mod blisp_expr;
mod blisp_parser;
mod blisp_eval;
mod blisp_func;

fn main() {
    println!("(+ 1 2) = {}", blisp_eval::evaluate(blisp_parser::parse(&mut blisp_lexer::lex("(+ 1 2)".to_string())), &blisp_func::default_env()));
}
