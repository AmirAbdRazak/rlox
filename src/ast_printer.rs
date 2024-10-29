use std::fmt;

use crate::{
    syntax::{BinaryExpr, CallExpr, Expr, Grouping, LogicalExpr, Stmt, UnaryExpr},
    visit::Visitor,
};

pub struct ASTStringVisitor<'a> {
    pub statements: &'a [Stmt],
}

impl<'a> Visitor for ASTStringVisitor<'a> {
    type E = String;
    type S = String;

    fn visit_expression(&self, expr: &Expr) -> String {
        match expr {
            Expr::Binary(BinaryExpr {
                left: left_expr,
                right: right_expr,
                operator,
            }) => format!(
                "(Binary {} {} {})",
                operator.token_type,
                self.visit_expression(left_expr),
                self.visit_expression(right_expr)
            ),
            Expr::Unary(UnaryExpr {
                operator,
                right: right_expr,
            }) => format!(
                "(Unary {} {})",
                operator.token_type,
                self.visit_expression(right_expr)
            ),
            Expr::Grouping(Grouping { expression: expr }) => {
                format!("(group {})", self.visit_expression(expr))
            }
            Expr::Literal(literal_value) => format!("(Literal {})", literal_value),
            Expr::Variable(variable) => format!("(Variable {})", variable.name),
            Expr::Assignment(assignment) => {
                format!(
                    "(Assignment {} = {})",
                    assignment.name, assignment.expression
                )
            }
            Expr::Logical(LogicalExpr {
                left: left_expr,
                right: right_expr,
                operator,
            }) => format!(
                "(Logical {} {} {})",
                operator.token_type,
                self.visit_expression(left_expr),
                self.visit_expression(right_expr)
            ),
            Expr::Call(CallExpr {
                ref callee,
                ref arguments,
                ..
            }) => format!(
                "(Callable <callee {}> <arguments {}>)",
                callee.to_string(),
                arguments.iter().map(|t| t.to_string()).collect::<String>()
            ),
        }
    }

    fn visit_statement(&self, s: &Stmt) -> String {
        match *s {
            Stmt::Expression(ref expr) => {
                format!("{}", self.visit_expression(expr))
            }
            Stmt::Print(ref expr) => format!("Print {}", self.visit_expression(expr)),
            Stmt::VariableDeclaration(_, ref expr) => {
                if let Some(ref expr) = expr {
                    format!("{}", self.visit_expression(expr))
                } else {
                    "nil".to_string()
                }
            }
            Stmt::Block(ref statements) => format!(
                "(Block Statement {})",
                statements
                    .iter()
                    .map(|s| self.visit_statement(s))
                    .collect::<String>()
            ),
            Stmt::If(ref conditional, ref then_stmt, ref else_stmt) => format!(
                "(If Statement {} {} {})",
                self.visit_expression(conditional),
                self.visit_statement(then_stmt),
                match else_stmt {
                    &Some(ref inner_else) => self.visit_statement(inner_else),
                    &None => String::from(""),
                }
            ),
            Stmt::While(ref conditional, ref body) => format!(
                "(If Statement {} {} )",
                self.visit_expression(conditional),
                self.visit_statement(body),
            ),
            Stmt::Function(ref name, ref params, ref body) => format!(
                "(Function {} {}) -> {}",
                name,
                params.iter().map(|t| t.to_string()).collect::<String>(),
                body.iter()
                    .map(|s| self.visit_statement(s))
                    .collect::<String>()
            ),
            Stmt::Return(ref _keyword, ref value) => {
                format!(
                    "<Return {}>",
                    match value {
                        Some(expr) => self.visit_expression(expr),
                        _ => "Nil".to_string(),
                    }
                )
            }
        }
    }
}

impl<'a> fmt::Display for ASTStringVisitor<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in self.statements {
            write!(f, "{}", self.visit_statement(statement))?;
        }

        Ok(())
    }
}
