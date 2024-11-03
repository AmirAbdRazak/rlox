use std::{
    collections::HashMap,
    fmt::{Display, Formatter, Result as FmtResult},
    mem,
};

use crate::{
    interpreter::Interpreter,
    syntax::{Expr, LambdaExpr, Stmt},
    token::{Token, TokenType},
    utils::hashmap_to_string,
    visit::MutVisitor,
};

#[derive(PartialEq)]
enum FunctionType {
    None,
    Function,
}

pub enum ResolverError {
    DuplicateVariableDeclaration(Token),
    TopFunctionReturn(usize),
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            ResolverError::DuplicateVariableDeclaration(name_token) => write!(
                f,
                "Can't assign to an already existing variable {}",
                name_token
            )?,
            ResolverError::TopFunctionReturn(line) => {
                write!(f, "Can't return from the top function at line {}", line)?
            }
        }

        Ok(())
    }
}

impl ResolverError {
    pub fn line(&self) -> usize {
        match self {
            ResolverError::TopFunctionReturn(line) => *line,
            ResolverError::DuplicateVariableDeclaration(name_token) => name_token.line,
        }
    }
}

pub type ResolverResult<T> = Result<T, ResolverError>;

pub struct Resolver<'main> {
    interpreter: &'main mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    function_type: FunctionType,
}

impl<'main> Resolver<'main> {
    pub fn new(interpreter: &'main mut Interpreter) -> Resolver<'main> {
        Resolver {
            interpreter,
            scopes: Vec::new(),
            function_type: FunctionType::None,
        }
    }
    pub fn resolve(&mut self, statements: &Vec<Stmt>) -> Vec<ResolverError> {
        statements
            .iter()
            .filter_map(|statement| match self.visit_statement(statement) {
                Err(err) => Some(err),
                _ => None,
            })
            .collect()
    }

    fn resolve_statements(&mut self, statements: &Vec<Stmt>) -> ResolverResult<()> {
        for statement in statements {
            self.visit_statement(statement)?
        }

        Ok(())
    }

    fn resolve_local(&mut self, expr: &Expr, name: &str) {
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                self.interpreter.resolve(expr, depth);
                return;
            }
        }
    }

    fn resolve_function(
        &mut self,
        lambda_expr: &LambdaExpr,
        mut function_type: FunctionType,
    ) -> ResolverResult<()> {
        mem::swap(&mut function_type, &mut self.function_type);
        self.begin_scope();
        for param in &lambda_expr.parameters {
            self.declare(param)?;
            self.define(param)?;
        }
        self.resolve_statements(&lambda_expr.body)?;
        println!(
            "current funct scope: {}",
            hashmap_to_string(self.scopes.last().unwrap())
        );
        self.end_scope();
        mem::swap(&mut function_type, &mut self.function_type);

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name_token: &Token) -> ResolverResult<()> {
        let current_scope = match self.scopes.last_mut() {
            Some(scope) => scope,
            None => return Ok(()),
        };

        if let TokenType::Identifier(ref name) = name_token.token_type {
            if current_scope.contains_key(name) {
                return Err(ResolverError::DuplicateVariableDeclaration(
                    name_token.clone(),
                ));
            }

            current_scope.insert(name.clone(), false);
        };

        Ok(())
    }

    fn define(&mut self, name_token: &Token) -> ResolverResult<()> {
        let len = self.scopes.len();
        let current_scope = match self.scopes.last_mut() {
            Some(scope) => scope,
            None => return Ok(()),
        };

        if let TokenType::Identifier(ref name) = name_token.token_type {
            println!("name {name_token} @ {len}");
            current_scope.insert(name.clone(), true);
        };

        Ok(())
    }
}

impl<'main> MutVisitor for Resolver<'main> {
    type E = ResolverResult<()>;
    type S = ResolverResult<()>;

    fn visit_statement(&mut self, statement: &Stmt) -> Self::S {
        match statement {
            Stmt::Block(statements) => {
                println!(
                    "In block: {}",
                    statements
                        .iter()
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>()
                        .join("\n")
                );
                self.begin_scope();
                self.resolve_statements(statements)?;
                self.end_scope();
            }
            Stmt::VariableDeclaration(ref name, ref initializer) => {
                self.declare(name)?;
                if let Some(expr) = initializer {
                    self.visit_expression(expr)?
                }
                self.define(name)?;
            }

            Stmt::Function(ref name_token, lambda) => {
                self.declare(name_token)?;
                self.define(name_token)?;
                self.resolve_function(lambda, FunctionType::Function)?;
            }
            Stmt::Expression(expr) => self.visit_expression(expr)?,
            Stmt::If(ref condition, ref then_branch, ref else_branch) => {
                self.visit_expression(condition)?;
                self.visit_statement(then_branch)?;
                if let Some(else_statement) = else_branch {
                    self.visit_statement(else_statement)?;
                }
            }
            Stmt::Print(print_expr) => self.visit_expression(print_expr)?,
            Stmt::Return(ref keyword, value) => {
                if self.function_type == FunctionType::None {
                    return Err(ResolverError::TopFunctionReturn(keyword.line));
                }

                if let Some(expr) = value {
                    self.visit_expression(expr)?;
                }
            }
            Stmt::While(condition, body) => {
                self.visit_expression(condition)?;
                self.visit_statement(body)?;
            }
        }
        Ok(())
    }

    fn visit_expression(&mut self, expr: &Expr) -> Self::E {
        match expr {
            Expr::Variable(ref variable_expr) => {
                let name = match &variable_expr.name.token_type {
                    TokenType::Identifier(name) => name,
                    _ => unreachable!(),
                };

                if let Some(false) = self.scopes.last().and_then(|scope| scope.get(name)) {
                    return Err(ResolverError::DuplicateVariableDeclaration(
                        variable_expr.name.clone(),
                    ));
                }

                self.resolve_local(expr, name)
            }
            Expr::Assignment(ref assignment_expr) => {
                let name = match &assignment_expr.name.token_type {
                    TokenType::Identifier(name) => name,
                    _ => unreachable!(),
                };
                self.visit_expression(&assignment_expr.expression)?;
                self.resolve_local(expr, name);
            }
            Expr::Binary(ref binary_expr) => {
                self.visit_expression(&binary_expr.left)?;
                self.visit_expression(&binary_expr.right)?;
            }
            Expr::Call(ref call_expr) => {
                self.visit_expression(&call_expr.callee)?;

                for argument in &call_expr.arguments {
                    self.visit_expression(argument)?;
                }
            }
            Expr::Grouping(ref grouping) => self.visit_expression(&grouping.expression)?,
            Expr::Literal(_) => (),
            Expr::Logical(logical_expr) => {
                self.visit_expression(&logical_expr.left)?;
                self.visit_expression(&logical_expr.right)?;
            }
            Expr::Unary(unary_expr) => self.visit_expression(&unary_expr.right)?,
            Expr::Lambda(lambda_expr) => self.resolve_statements(&lambda_expr.body)?,
        }
        Ok(())
    }
}
