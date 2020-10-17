
use std::io::{
    self,
    Write,
};

use std::env;
use std::fs;

mod blisp_lexer;
mod blisp_expr;
mod blisp_parser;
mod blisp_eval;
mod blisp_func;

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        repl();

        Ok(())
    } else {
        let filename = &args[1];
        let program: String = fs::read_to_string(filename)?.parse()?;

        println!("{}", blisp_eval::evaluate(blisp_parser::parse(&mut blisp_lexer::lex(program)), std::rc::Rc::new(blisp_func::default_env())));

        Ok(())
    }
}

fn repl() {
    loop {
        print!(">");
        io::stdout().flush().unwrap();
        
        let mut line = String::new();
        io::stdin().read_line(&mut line).expect("Failure reading line");

        println!("{}", blisp_eval::evaluate(blisp_parser::parse(&mut blisp_lexer::lex(line)), std::rc::Rc::new(blisp_func::default_env())));
    }
}

