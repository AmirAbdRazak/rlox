use std::fmt;

use crate::{
    syntax::{BinaryExpr, Expr, Grouping, Stmt, UnaryExpr},
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
                "({} {} {})",
                operator.token_type,
                self.visit_expression(left_expr),
                self.visit_expression(right_expr)
            ),
            Expr::Unary(UnaryExpr {
                operator,
                right: right_expr,
            }) => format!(
                "({} {})",
                operator.token_type,
                self.visit_expression(right_expr)
            ),
            Expr::Grouping(Grouping { expression: expr }) => {
                format!("(group {})", self.visit_expression(expr))
            }
            Expr::Literal(literal_value) => format!("{}", literal_value),
            Expr::Variable(variable) => format!("{}", variable.name),
            Expr::Assignment(assignment) => {
                format!("{} = {}", assignment.name, assignment.expression)
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
