use std::{fmt, rc::Rc};

use crate::{ast_printer::ASTStringVisitor, token::Token};

#[derive(Clone, PartialEq, Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}
#[derive(Clone, PartialEq, Debug)]
pub struct Grouping {
    pub expression: Box<Expr>,
}
#[derive(Clone, PartialEq, Debug)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum LiteralValue {
    Float(String),
    LoxString(String),
    Bool(bool),
    None,
}
#[derive(Clone, PartialEq, Debug)]
pub struct VariableExpr {
    pub id: usize,
    pub name: Token,
}
#[derive(Clone, PartialEq, Debug)]
pub struct AssignmentExpr {
    pub id: usize,
    pub name: Token,
    pub expression: Box<Expr>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct LogicalExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub closing_paren: Token,
    pub arguments: Vec<Box<Expr>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct LambdaExpr {
    pub parameters: Vec<Token>,
    pub body: Vec<Stmt>,
}

impl fmt::Display for LambdaExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "<function {}> -> {}",
            self.parameters
                .iter()
                .map(|t| t.to_string())
                .collect::<String>(),
            self.body.iter().map(|s| s.to_string()).collect::<String>()
        )?;

        Ok(())
    }
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

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Binary(BinaryExpr),
    Grouping(Grouping),
    Literal(LiteralValue),
    Unary(UnaryExpr),
    Variable(VariableExpr),
    Assignment(AssignmentExpr),
    Logical(LogicalExpr),
    Call(CallExpr),
    Lambda(Rc<LambdaExpr>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    VariableDeclaration(Token, Option<Expr>),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Function(Token, Rc<LambdaExpr>),
    Return(Token, Option<Expr>),
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
