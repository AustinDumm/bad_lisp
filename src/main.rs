
use std::io::{
    self,
    Write,
};

use std::fs;

use clap::{App, Arg, ArgGroup, ArgMatches};

mod blisp_lexer;
mod blisp_parser;
mod blisp_eval;
mod blisp_func;
mod types;

use types::BLispEvalResult;

fn clap_args() -> ArgMatches {
    App::new("bad_lisp")
        .version("1.0")
        .author("Austin Dumm <dummmagic@gmail.com>")
        .about("Interpreter tool for bad_lisp language")
        .group(ArgGroup::new("Halt")
               .args(&["lex", "parse"]))
        .arg(Arg::new("lex")
             .short('l')
             .long("lex")
             .takes_value(false)
             .conflicts_with("parse"))
        .arg(Arg::new("parse")
             .short('p')
             .long("parse")
             .takes_value(false))
        .arg(Arg::new("file")
             .short('f')
             .long("file")
             .takes_value(true)).get_matches()
}

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let matches = clap_args();
    let filename = matches.value_of("file");

    let pipeline = match (matches.is_present("lex"), matches.is_present("parse")) {
        (true, _) => |program: String| {
            match blisp_lexer::lex(program) {
                Ok(list) => format!("{:?}", list),
                Err(error) => format!("{}", error),
            }
        },
        (_, true) => |program: String| {
            match blisp_lexer::lex(program) {
                Ok(mut list) =>
                    match blisp_parser::parse(&mut list) {
                        Ok(ast) => format!("{}", ast),
                        Err(error) => format!("{}", error),
                    },
                Err(error) => format!("{}", error),
            }
        },
        (_, _) => |program: String| {
            match blisp_lexer::lex(program) {
                Ok(mut list) => 
                    match blisp_parser::parse(&mut list) {
                        Ok(ast) =>
                            match blisp_eval::evaluate(ast, std::rc::Rc::new(blisp_func::default_env())) {
                                BLispEvalResult::Result(result) => format!("{}", result),
                                BLispEvalResult::Error(error) => format!("{}", error),
                                BLispEvalResult::TailCall(_, _) => panic!("TailCall returned from evaluate"),
                                BLispEvalResult::Stack(_) => panic!("Stack returned from top-level evaluate"),
                            }
                        Err(error) => format!("{}", error),
                    }
                Err(error) => format!("{}", error),
            }
        },
    };

    match filename {
        None => repl(pipeline),
        Some(filename) => from_file(pipeline, filename.to_string()),
    }
    
    Ok(())
}

fn from_file(pipeline: fn(String) -> String, filename: String) {
    let program: String = fs::read_to_string(filename).expect("Failed to open file").parse().expect("Failed to parse");
    let result = pipeline(program);
    println!("{}", result);
}

fn repl(pipeline: fn(String) -> String) {
    loop {
        print!(">");
        io::stdout().flush().unwrap();
        
        let mut line = String::new();
        io::stdin().read_line(&mut line).expect("Failure reading line");

        println!("{}", pipeline(line));
    }
}

