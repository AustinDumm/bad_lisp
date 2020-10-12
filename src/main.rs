
use std::io::{
    self,
    Write,
};

mod blisp_lexer;
mod blisp_expr;
mod blisp_parser;
mod blisp_eval;
mod blisp_func;

fn main() {
    loop {
        print!(">");
        io::stdout().flush().unwrap();
        
        let mut line = String::new();
        io::stdin().read_line(&mut line).expect("Failure reading line");
        if line.contains("--quit") {
            break;
        }

        println!("{}", blisp_eval::evaluate(blisp_parser::parse(&mut blisp_lexer::lex(line)), &blisp_func::default_env()));
    }
}
