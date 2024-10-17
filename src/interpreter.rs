use crate::syntax::{
    AssignmentExpr, BinaryExpr, Expr, Grouping, LiteralValue, Stmt, UnaryExpr, VariableExpr,
};
use crate::token::{Token, TokenType as TT};
use crate::visit::MutVisitor;
use std::collections::HashMap;
use std::fmt;
use std::mem::discriminant;

pub enum RuntimeError {
    IncompatibleBinaryOperation(Types, Types, TT, usize),
    IncompatibleUnaryOperation(Types, TT, usize),
    NullDivisionError(usize),
    UndefinedVariable(String, usize),
    InvalidAssignment(String, usize),
}
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::IncompatibleBinaryOperation(
                left_type,
                right_type,
                operation_token,
                _,
            ) => {
                write!(
                    f,
                    "Incompatible operation: <{} {} {}> ",
                    left_type, operation_token, right_type
                )?;
            }
            RuntimeError::IncompatibleUnaryOperation(right_type, operation_token, _line) => {
                match operation_token {
                    _ => write!(
                        f,
                        "Incompatible operation: <{}{}>",
                        operation_token, right_type
                    )?,
                }
            }
            RuntimeError::NullDivisionError(_) => {
                write!(f, "Division by zero is not supported")?;
            }
            RuntimeError::UndefinedVariable(var_name, _) => {
                write!(f, "Undefined Variable : {var_name}")?;
            }
            RuntimeError::InvalidAssignment(name, _) => {
                write!(f, "Invalid Assignment: {name}")?;
            }
        }
        Ok(())
    }
}

impl RuntimeError {
    pub fn line(&self) -> usize {
        match *self {
            RuntimeError::IncompatibleBinaryOperation(_, _, _, line) => line,
            RuntimeError::IncompatibleUnaryOperation(_, _, line) => line,
            RuntimeError::NullDivisionError(line) => line,
            RuntimeError::UndefinedVariable(_, line) => line,
            RuntimeError::InvalidAssignment(_, line) => line,
        }
    }
}

type RuntimeResult<T> = Result<T, RuntimeError>;

pub struct Environment {
    values: HashMap<String, Types>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    fn extract_identifier(&self, token: &TT) -> String {
        if let TT::Identifier(name) = token {
            name.to_string()
        } else {
            panic!("Only pass Identifier tokens here")
        }
    }

    pub fn define(&mut self, token: &Token, value: Types) {
        let name = self.extract_identifier(&token.token_type);
        self.values.insert(name, value);
    }

    pub fn get(&self, token: &Token) -> RuntimeResult<Types> {
        let name = self.extract_identifier(&token.token_type);
        self.values
            .get(&name)
            .ok_or(RuntimeError::UndefinedVariable(name, token.line))
            .cloned()
    }

    pub fn assign(&mut self, name_token: &Token, value: Types) -> RuntimeResult<Types> {
        let name = self.extract_identifier(&name_token.token_type);
        if self.values.contains_key(&name) {
            self.values.insert(name, value.clone());
            Ok(value)
        } else {
            Err(RuntimeError::InvalidAssignment(name, name_token.line))
        }
    }
}

pub struct Interpreter {
    pub environment: Environment,
    had_runtime_error: bool,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Environment::new(),
            had_runtime_error: false,
        }
    }
    pub fn interpret(&mut self, program: &[Stmt]) -> (Vec<Types>, Vec<RuntimeError>) {
        let mut evals = vec![];
        let mut runtime_errors = vec![];

        for statement in program {
            match self.visit_statement(statement) {
                Ok(eval) => evals.push(eval),
                Err(runtime_error) => {
                    runtime_errors.push(runtime_error);
                    self.had_runtime_error = true;
                    break;
                }
            }
        }

        (evals, runtime_errors)
    }
}

impl MutVisitor for Interpreter {
    type E = Result<Types, RuntimeError>;
    type S = Result<Types, RuntimeError>;

    fn visit_expression(&mut self, expr: &Expr) -> Self::E {
        match expr {
            Expr::Binary(BinaryExpr {
                left: ref left_expr,
                ref operator,
                right: ref right_expr,
            }) => {
                let right = self.visit_expression(right_expr)?;
                let left = self.visit_expression(left_expr)?;

                let error = RuntimeError::IncompatibleBinaryOperation(
                    left.clone(),
                    right.clone(),
                    operator.token_type.clone(),
                    operator.line,
                );

                match (&left, &operator.token_type, &right) {
                    (left, token_type, right)
                        if discriminant(left) != discriminant(right)
                            && TT::equality_tokens().contains(token_type) =>
                    {
                        Ok(Types::Boolean(false))
                    }
                    (&Types::Number(_), token_type, &Types::LoxString(_)) => match token_type {
                        _ if TT::equality_tokens().contains(token_type) => {
                            Ok(Types::Boolean(false))
                        }
                        _ => Err(error),
                    },
                    (&Types::LoxString(_), token_type, &Types::Number(_)) => match token_type {
                        _ if TT::equality_tokens().contains(token_type) => {
                            Ok(Types::Boolean(false))
                        }
                        _ => Err(error),
                    },
                    (&Types::LoxString(ref left_s), token_type, &Types::LoxString(ref right_s)) => {
                        match token_type {
                            TT::Plus => Ok(Types::LoxString(format!("{}{}", left_s, right_s))),
                            TT::EqualEqual => Ok(Types::Boolean(left_s == right_s)),
                            TT::Greater => Ok(Types::Boolean(left_s > right_s)),
                            TT::GreaterEqual => Ok(Types::Boolean(left_s >= right_s)),
                            TT::Less => Ok(Types::Boolean(left_s < right_s)),
                            TT::LessEqual => Ok(Types::Boolean(left_s <= right_s)),
                            TT::BangEqual => Ok(Types::Boolean(left_s != right_s)),
                            _ => Err(error),
                        }
                    }

                    (&Types::Number(left_n), token_type, &Types::Number(right_n)) => {
                        match token_type {
                            TT::Plus => Ok(Types::Number(left_n + right_n)),
                            TT::Minus => Ok(Types::Number(left_n - right_n)),
                            TT::Star => Ok(Types::Number(left_n * right_n)),
                            TT::Slash => {
                                if right_n == 0.0 {
                                    Err(RuntimeError::NullDivisionError(operator.line))
                                } else {
                                    Ok(Types::Number(left_n / right_n))
                                }
                            }
                            TT::Greater => Ok(Types::Boolean(left_n > right_n)),
                            TT::GreaterEqual => Ok(Types::Boolean(left_n >= right_n)),
                            TT::Less => Ok(Types::Boolean(left_n < right_n)),
                            TT::LessEqual => Ok(Types::Boolean(left_n <= right_n)),
                            TT::EqualEqual => Ok(Types::Boolean(left_n == right_n)),
                            TT::BangEqual => Ok(Types::Boolean(left_n != right_n)),
                            _ => Err(error),
                        }
                    }
                    (&Types::Nil, token_type, &Types::Nil) => match token_type {
                        TT::EqualEqual => Ok(Types::Boolean(true)),
                        TT::BangEqual => Ok(Types::Boolean(false)),
                        _ => Err(error),
                    },
                    (&Types::Boolean(left_b), token_type, &Types::Boolean(right_b)) => {
                        match token_type {
                            TT::EqualEqual => Ok(Types::Boolean(left_b == right_b)),
                            TT::BangEqual => Ok(Types::Boolean(left_b != right_b)),
                            _ => Err(error),
                        }
                    }

                    _ => Err(error),
                }
            }
            Expr::Unary(UnaryExpr {
                operator,
                right: right_expr,
            }) => {
                let right = self.visit_expression(right_expr)?;

                match (&right, &operator.token_type) {
                    (Types::Number(n), &TT::Minus) => Ok(Types::Number(-n)),
                    (Types::Nil, &TT::Bang) | (Types::Boolean(false), TT::Bang) => {
                        Ok(Types::Boolean(true))
                    }
                    (_, &TT::Bang) => Ok(Types::Boolean(false)),
                    _ => Err(RuntimeError::IncompatibleUnaryOperation(
                        right,
                        operator.token_type.clone(),
                        operator.line,
                    )),
                }
            }
            Expr::Literal(lit_val) => {
                let ret_val = match lit_val {
                    LiteralValue::Bool(val) => Types::Boolean(*val),
                    LiteralValue::Float(val) => Types::Number(val.parse().unwrap()),
                    LiteralValue::LoxString(val) => Types::LoxString(val.clone()),
                    LiteralValue::None => Types::Nil,
                };
                Ok(ret_val)
            }
            Expr::Grouping(Grouping { expression }) => self.visit_expression(expression),
            Expr::Variable(VariableExpr { id: _, ref name }) => self.environment.get(name),
            Expr::Assignment(AssignmentExpr {
                id: _,
                ref name,
                ref expression,
            }) => {
                let value = self.visit_expression(expression)?;
                self.environment.assign(name, value)
            }
        }
    }

    fn visit_statement(&mut self, statement: &Stmt) -> Self::S {
        match statement {
            Stmt::Expression(ref expr) => {
                let evaluation = self.visit_expression(expr)?;
                Ok(evaluation)
            }
            Stmt::Print(ref expr) => {
                let evaluation = self.visit_expression(expr)?;
                if !self.had_runtime_error {
                    println!("{evaluation}");
                }
                Ok(Types::Nil)
            }
            Stmt::VariableDeclaration(ref token, ref initializer) => {
                let value = initializer
                    .as_ref()
                    .map(|expr| self.visit_expression(expr))
                    .transpose()?
                    .unwrap_or(Types::Nil);

                self.environment.define(token, value);
                Ok(Types::Nil)
            }
        }
    }
}

#[derive(Clone)]
pub enum Types {
    Number(f32),
    LoxString(String),
    Boolean(bool),
    Nil,
}

impl std::fmt::Display for Types {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &Types::Boolean(b) => write!(f, "{}", b),
            &Types::LoxString(ref s) => write!(f, "{}", s.to_string()),
            &Types::Nil => write!(f, "nil"),
            &Types::Number(n) => write!(f, "{}", n),
        }
    }
}

fn _is_truthy(expression_return: &Types) -> bool {
    match expression_return {
        &Types::Nil | &Types::Boolean(false) => false,
        _ => true,
    }
}
