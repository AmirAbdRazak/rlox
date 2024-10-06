use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(String),
    LoxString(String),
    Number(f64),
    Nil,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Or,
    Return,
    Super,
    This,
    True,
    Var,
    Print,
    While,

    Eof,
}

pub struct Token {
    pub token_type: TokenType,
    pub _line: usize,
}

impl Token {
    pub fn _new(token_type: TokenType, line: usize) -> Token {
        Token {
            token_type,
            _line: line,
        }
    }
    pub fn token_type_name(&self) -> String {
        format!("{:?}", self.token_type).chars().enumerate().fold(
            String::new(),
            |mut acc, (i, c)| {
                if c.is_uppercase() && i != 0 {
                    acc.push('_'); // Insert underscore before uppercase (except the first character)
                }
                acc.push(c.to_ascii_uppercase()); // Push the uppercase version of the character
                acc
            },
        )
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.token_type {
            TokenType::LoxString(ref s) => write!(f, "STRING \"{}\" {}", s, s)?,
            _ => write!(f, "{} {} null", self.token_type_name(), self.token_type)?,
        };
        Ok(())
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenType::*;
        let value: &str = match self {
            LeftParen => "(",
            RightParen => ")",
            LeftBrace => "{",
            RightBrace => "}",
            Comma => ",",
            Dot => ".",
            Minus => "-",
            Plus => "+",
            Semicolon => ";",
            Slash => "/",
            Star => "*",

            // One or two character tokens.
            Bang => "!",
            BangEqual => "!=",
            Equal => "=",
            EqualEqual => "==",
            Greater => ">",
            GreaterEqual => ">=",
            Less => "<",
            LessEqual => "<=",

            // Literals.
            Identifier(identifier) => identifier,
            LoxString(literal) => literal,
            Number(f64) => &f64.to_string(),
            Nil => "Nil",

            // Keywords.
            And => "AND",
            Class => "CLASS",
            Else => "ELSE",
            False => "FALSE",
            Fun => "FUN",
            For => "FOR",
            If => "IF",
            Or => "OR",
            Print => "PRINT",
            Return => "RETURN",
            Super => "SUPER",
            This => "THIS",
            True => "TRUE",
            Var => "VAR",
            While => "WHILE",

            Eof => "",
        };

        write!(f, "{value}")?;

        Ok(())
    }
}
