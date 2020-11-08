
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

use blisp_expr::BLispEvalResult;

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        repl();

        Ok(())
    } else {
        let filename = &args[1];
        let program: String = fs::read_to_string(filename)?.parse()?;

        match blisp_lexer::lex(program) {
            Ok(mut list) => 
                match blisp_parser::parse(&mut list) {
                    Ok(ast) => 
                        match blisp_eval::evaluate(ast, std::rc::Rc::new(blisp_func::default_env())) {
                            BLispEvalResult::Result(result) => println!("{}", result),
                            BLispEvalResult::Error(error) => println!("{}", error),
                            BLispEvalResult::TailCall(_, _) => panic!("TailCall returned from evaluate"),
                        }
                    Err(error) => println!("{}", error),
                }
            Err(error) => println!("{}", error),
        }

        Ok(())
    }
}

fn repl() {
    loop {
        print!(">");
        io::stdout().flush().unwrap();
        
        let mut line = String::new();
        io::stdin().read_line(&mut line).expect("Failure reading line");

        match blisp_lexer::lex(line) {
            Ok(mut list) => 
                match blisp_parser::parse(&mut list) {
                    Ok(ast) => 
                        match blisp_eval::evaluate(ast, std::rc::Rc::new(blisp_func::default_env())) {
                            BLispEvalResult::Result(result) => println!("{}", result),
                            BLispEvalResult::Error(error) => println!("{}", error),
                            BLispEvalResult::TailCall(_, _) => panic!("TailCall returned from evaluate"),
                        }
                    Err(error) => println!("{}", error),
                }
            Err(error) => println!("{}", error),
        }
    }
}

