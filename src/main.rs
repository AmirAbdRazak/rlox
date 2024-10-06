use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::exit;

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
                let mut runtime = Lox::new();
                runtime.run(file_contents);

                if runtime.had_error {
                    exit(65);
                }
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

pub struct Lox {
    had_error: bool,
}
impl Lox {
    pub fn new() -> Lox {
        Lox { had_error: false }
    }

    pub fn run(&mut self, source: String) {
        let mut scanner: scanner::Scanner = scanner::Scanner::new(&source);
        let (tokens, errors) = scanner.scan_tokens();

        errors
            .iter()
            .for_each(|err| self.report(err.line(), format!("{}", err)));
        tokens.iter().for_each(|token| println!("{token}"));
    }

    pub fn report(&mut self, line: usize, message: String) {
        eprintln!("[line {}] Error: {}", line, message);
        self.had_error = true;
    }
}
