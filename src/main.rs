use std::env;
use std::fs;
use std::io::{self, Write};

use token::Token;

mod lexer;
mod scanner;
mod token;

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

            if !file_contents.is_empty() {
                let mut scanner: scanner::Scanner = scanner::Scanner::new(&file_contents);
                let mut tokens: Vec<Token> = Vec::new();
                match scanner.scan_tokens() {
                    Ok(ts) => tokens = ts,
                    Err(errors) => errors
                        .iter()
                        .for_each(|err| report(err.line(), format!("{}", err))),
                };

                tokens.iter().for_each(|token| println!("{token}"));
            } else {
                println!("EOF  null"); // Placeholder, remove this line when implementing the scanner
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}

pub fn report(line: usize, message: String) {
    println!("[line {}] Error: {}", line, message);
}
