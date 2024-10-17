use std::{collections::HashMap, fmt, iter::Peekable, str::Chars};

use crate::token::{Token, TokenType};

struct Keyword(HashMap<&'static str, TokenType>);
impl Keyword {
    pub fn new() -> Keyword {
        let mut keywords = HashMap::new();

        use crate::token::TokenType::*;
        keywords.insert("or", Or);
        keywords.insert("and", And);
        keywords.insert("if", If);
        keywords.insert("else", Else);
        keywords.insert("var", Var);
        keywords.insert("for", For);
        keywords.insert("while", While);
        keywords.insert("fun", Fun);
        keywords.insert("class", Class);
        keywords.insert("super", Super);
        keywords.insert("this", This);
        keywords.insert("return", Return);
        keywords.insert("true", True);
        keywords.insert("false", False);
        keywords.insert("nil", Nil);
        keywords.insert("print", Print);

        Keyword(keywords)
    }
}
pub type ScannerResult<T> = Result<T, ScannerError>;

#[derive(Debug)]
pub enum ScannerError {
    UnknownCharacter(char, usize),
    UnterminatedString(usize),
    UnparseableDigit(String, usize),
}

impl fmt::Display for ScannerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScannerError::UnknownCharacter(c, _line) => {
                write!(f, "Unexpected character: {c}")?;
            }
            ScannerError::UnterminatedString(_line) => {
                write!(f, "Unterminated string.")?;
            }
            ScannerError::UnparseableDigit(err_str, line) => {
                write!(f, "Unparseable digit {} at line {}", err_str, line)?;
            }
        }

        Ok(())
    }
}

impl ScannerError {
    pub fn line(&self) -> usize {
        match *self {
            ScannerError::UnknownCharacter(_, line) => line,
            ScannerError::UnterminatedString(line) => line,
            ScannerError::UnparseableDigit(_, line) => line,
        }
    }
}

pub struct Scanner<'a> {
    source: Peekable<Chars<'a>>,
    line: usize,
    keywords: Keyword,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source: source.chars().peekable(),
            line: 1,
            keywords: Keyword::new(),
        }
    }

    pub fn scan_tokens(&mut self) -> (Vec<Token>, Vec<ScannerError>) {
        let mut tokens: Vec<Token> = Vec::new();
        let mut errors: Vec<ScannerError> = Vec::new();

        loop {
            self.skip_whitespace();
            let ch = self.source.next();
            match ch {
                Some(ch) => {
                    if !self.skip_comments(ch) {
                        match self.scan_token(ch) {
                            Ok(token) => tokens.push(token),
                            Err(err) => errors.push(err),
                        }
                    }
                }
                None => {
                    tokens.push(self.simple_token(TokenType::Eof));
                    break;
                }
            };
        }

        (tokens, errors)
    }

    pub fn simple_token(&self, token_type: TokenType) -> Token {
        Token {
            token_type,
            line: self.line,
        }
    }

    pub fn scan_operator(
        &mut self,
        token_type: TokenType,
        equality_token_type: TokenType,
    ) -> Token {
        if self.source.peek() == Some(&'=') {
            // Consume the =
            self.source.next();
            self.simple_token(equality_token_type)
        } else {
            self.simple_token(token_type)
        }
    }

    pub fn skip_comments(&mut self, ch: char) -> bool {
        if ch == '/' && self.source.peek() == Some(&'/') {
            while let Some(&c) = self.source.peek() {
                self.source.next();
                if c == '\n' {
                    self.line += 1;
                    break;
                }
            }

            return true;
        }

        return false;
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(&c) = self.source.peek() {
            if c.is_whitespace() {
                if c == '\n' {
                    self.line += 1
                }

                self.source.next();
            } else {
                return;
            }
        }
    }

    pub fn parse_string(&mut self) -> ScannerResult<Token> {
        let mut string = String::new();
        let line = self.line;

        while let Some(&c) = self.source.peek() {
            if c == '"' {
                self.source.next();
                return Ok(self.simple_token(TokenType::LoxString(string)));
            } else if c == '\n' {
                self.line += 1;
            }
            string.push(self.source.next().unwrap());
        }

        return Err(ScannerError::UnterminatedString(line));
    }

    pub fn consume_string<F>(&mut self, string: &mut String, char_pred: F)
    where
        F: Fn(char) -> bool,
    {
        while let Some(&c) = self.source.peek() {
            if char_pred(c) {
                self.source.next();
                string.push(c);
            } else {
                break;
            }
        }
    }

    pub fn parse_number(&mut self, ch: char) -> ScannerResult<Token> {
        let mut string = String::new();
        string.push(ch);

        self.consume_string(&mut string, char::is_numeric);

        // Get the leading number
        if let Some(&c) = self.source.peek() {
            if c == '.' {
                self.source.next();
                string.push(c);
                self.consume_string(&mut string, char::is_numeric)
            }
        }

        match string.parse::<f32>() {
            Ok(_float) => Ok(self.simple_token(TokenType::Number(string))),
            Err(_) => {
                return Err(ScannerError::UnparseableDigit(string, self.line));
            }
        }
    }

    pub fn parse_identifier(&mut self, ch: char) -> ScannerResult<Token> {
        let mut string = String::new();
        string.push(ch);

        while let Some(&c) = self.source.peek() {
            if !c.is_alphabetic() && c != '_' && !c.is_digit(10) {
                break;
            }
            string.push(c);
            self.source.next();
        }

        match self.keywords.0.get(string.as_str()) {
            Some(keyword_type) => Ok(self.simple_token(keyword_type.clone())),
            None => Ok(self.simple_token(TokenType::Identifier(string))),
        }
    }

    pub fn scan_token(&mut self, ch: char) -> ScannerResult<Token> {
        use crate::token::TokenType::*;
        let token = match ch {
            '(' => self.simple_token(LeftParen),
            ')' => self.simple_token(RightParen),
            '{' => self.simple_token(LeftBrace),
            '}' => self.simple_token(RightBrace),
            ',' => self.simple_token(Comma),
            '.' => self.simple_token(Dot),
            '-' => self.simple_token(Minus),
            '+' => self.simple_token(Plus),
            ';' => self.simple_token(Semicolon),
            '*' => self.simple_token(Star),

            '!' => self.scan_operator(Bang, BangEqual),
            '=' => self.scan_operator(Assignment, EqualEqual),
            '<' => self.scan_operator(Less, LessEqual),
            '>' => self.scan_operator(Greater, GreaterEqual),

            '/' => self.simple_token(Slash),

            '"' => return self.parse_string(),

            ch @ _ => {
                if ch.is_digit(10) {
                    return self.parse_number(ch);
                } else if ch.is_alphabetic() || ch == '_' {
                    return self.parse_identifier(ch);
                } else {
                    return Err(ScannerError::UnknownCharacter(ch, self.line));
                }
            }
        };

        Ok(token)
    }
}
