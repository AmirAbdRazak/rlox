use std::fmt;

use crate::{ast_printer::ASTStringVisitor, token::Token};

// #[derive(Debug, Clone)]
// pub enum UnaryOperator {
//     Bang,
//     Minus,
// }
//
// #[derive(Debug, Clone)]
// pub enum BinaryOperator {
//     Minus,
//     Plus,
//     Slash,
//     Star,
//     Equal,
//     NotEqual,
//     Less,
//     LessEqual,
//     Greater,
//     GreaterEqual,
// }

#[derive(Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}
#[derive(Clone)]
pub struct Grouping {
    pub expression: Box<Expr>,
}
#[derive(Clone)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone)]
pub enum LiteralValue {
    Float(String),
    LoxString(String),
    Bool(bool),
    None,
}
#[derive(Clone)]
pub struct VariableExpr {
    pub id: usize,
    pub name: Token,
}

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = match self {
            LiteralValue::Float(n) => {
                let parsed_n = n.parse::<f32>().expect("Already parsed the number");
                if parsed_n.fract() == 0.0 {
                    format!("{:.1}", parsed_n)
                } else {
                    format!("{}", parsed_n)
                }
            }
            LiteralValue::None => "nil".to_string(),
            LiteralValue::Bool(bool) => bool.to_string(),
            LiteralValue::LoxString(literal) => literal.to_owned(),
        };

        write!(f, "{}", value)?;

        Ok(())
    }
}

impl From<String> for LiteralValue {
    fn from(value: String) -> Self {
        LiteralValue::LoxString(value)
    }
}

impl From<bool> for LiteralValue {
    fn from(value: bool) -> Self {
        LiteralValue::Bool(value)
    }
}

#[derive(Clone)]
pub enum Expr {
    Binary(BinaryExpr),
    Grouping(Grouping),
    Literal(LiteralValue),
    Unary(UnaryExpr),
    Variable(VariableExpr),
}

#[derive(Clone)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    VariableDeclaration(Token, Option<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let expr_str = ASTStringVisitor {
            statements: &[Stmt::Expression(self.clone())],
        };
        write!(f, "{}", expr_str)?;

        Ok(())
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let expr_str = ASTStringVisitor {
            statements: &[self.clone()],
        };
        write!(f, "{}", expr_str)?;

        Ok(())
    }
}
