use std::fmt;

use crate::{
    syntax::{Expr, Stmt},
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
            Expr::Binary(binary_expr) => format!(
                "(Binary {} {} {})",
                binary_expr.operator.token_type,
                self.visit_expression(&binary_expr.left),
                self.visit_expression(&binary_expr.right)
            ),
            Expr::Unary(unary_expr) => format!(
                "(Unary {} {})",
                unary_expr.operator.token_type,
                self.visit_expression(&unary_expr.right)
            ),
            Expr::Grouping(grouping_expr) => {
                format!(
                    "(group {})",
                    self.visit_expression(&grouping_expr.expression)
                )
            }
            Expr::Literal(literal_expr) => format!("(Literal {})", literal_expr.literal),
            Expr::Variable(variable) => format!("(Variable {})", variable.name),
            Expr::Assignment(assignment) => {
                format!(
                    "(Assignment {} = {})",
                    assignment.name, assignment.expression
                )
            }
            Expr::Logical(logical_expr) => format!(
                "(Logical {} {} {})",
                logical_expr.operator.token_type,
                self.visit_expression(&logical_expr.left),
                self.visit_expression(&logical_expr.right)
            ),
            Expr::Call(call_expr) => format!(
                "(Callable <callee {}> <arguments {}>)",
                call_expr.callee,
                call_expr
                    .arguments
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<String>()
            ),
            Expr::Lambda(lambda) => {
                format!(
                    "(Lambda {}) -> {}",
                    lambda
                        .parameters
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<String>(),
                    lambda
                        .body
                        .iter()
                        .map(|s| self.visit_statement(s))
                        .collect::<String>()
                )
            }
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
            Stmt::Function(ref name, ref function) => {
                format!("<Function {} -> {}>", name, function)
            }
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
