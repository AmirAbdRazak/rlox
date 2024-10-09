use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::exit;

use parser::ParserMode;
use token::Token;

mod ast_printer;
mod parser;
mod scanner;
mod syntax;
mod token;
mod visit;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut runtime = Lox::new(file_contents, LoxMode::Tokenize);
            let _ = runtime.tokenize();
        }
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut runtime = Lox::new(file_contents, LoxMode::Parse);
            runtime.parse();

            if runtime.had_error {
                exit(65);
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}

pub enum LoxMode {
    Tokenize,
    Parse,
}

pub struct Lox {
    source: String,
    had_error: bool,
    mode: LoxMode,
}
impl Lox {
    pub fn new(source: String, mode: LoxMode) -> Lox {
        Lox {
            source,
            had_error: false,
            mode,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut scanner: scanner::Scanner = scanner::Scanner::new(&self.source);
        let (tokens, errors) = scanner.scan_tokens();

        match self.mode {
            LoxMode::Tokenize => {
                errors
                    .iter()
                    .for_each(|err| self.report(err.line(), format!("{}", err)));
                tokens.iter().for_each(|token| println!("{token}"));
            }
            _ => {}
        }

        if self.had_error {
            exit(65);
        }

        tokens
    }
    pub fn parse(&mut self) {
        let tokens = self.tokenize();
        let mut parser = parser::Parser::new(tokens, ParserMode::Expression);
        let (statements, errors) = parser.parse();

        match self.mode {
            LoxMode::Parse => statements.iter().for_each(|expr| println!("{expr}")),
            _ => {}
        }
    }

    pub fn report(&mut self, line: usize, message: String) {
        eprintln!("[line {}] Error: {}", line, message);
        self.had_error = true;
    }
}
