use crate::syntax::{Expr, Stmt};

pub trait MutVisitor {
    type E;
    type S;

    fn visit_expression(&mut self, expr: &Expr) -> Self::E;
    fn visit_statement(&mut self, statement: &Stmt) -> Self::S;
}

pub trait Visitor {
    type E;
    type S;

    fn visit_expression(&self, expr: &Expr) -> Self::E;
    fn visit_statement(&self, statement: &Stmt) -> Self::S;
}
