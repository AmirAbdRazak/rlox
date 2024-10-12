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
    Number(String),
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

impl TokenType {
    pub fn equality_tokens() -> [TokenType; 2] {
        [TokenType::BangEqual, TokenType::EqualEqual]
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
}

impl Token {
    pub fn _new(token_type: TokenType, line: usize) -> Token {
        Token { token_type, line }
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
            TokenType::Number(ref n) => {
                let parsed_n = n.parse::<f32>().expect("Already parsed the number");
                if parsed_n.fract() == 0.0 {
                    write!(f, "NUMBER {} {:.1}", n, parsed_n)?
                } else {
                    write!(f, "NUMBER {} {}", n, parsed_n)?
                }
            }
            TokenType::LoxString(ref s) => write!(f, "STRING \"{}\" {}", s, s)?,
            TokenType::Identifier(ref i) => write!(f, "IDENTIFIER {} null", i)?,
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
            Nil => "nil",

            // Keywords.
            And => "and",
            Class => "class",
            Else => "else",
            False => "false",
            Fun => "fun",
            For => "for",
            If => "if",
            Or => "or",
            Print => "print",
            Return => "return",
            Super => "super",
            This => "this",
            True => "true",
            Var => "var",
            While => "while",

            Eof => "",
        };

        write!(f, "{value}")?;

        Ok(())
    }
}
