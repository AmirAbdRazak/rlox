use crate::syntax::{Expr, LambdaExpr, LiteralValue, Stmt};
use crate::token::{Token, TokenType as TT};
use crate::visit::MutVisitor;
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::mem::discriminant;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};
use std::{fmt, mem};

pub enum RuntimeError {
    IncompatibleBinaryOperation(Types, Types, TT, usize),
    IncompatibleUnaryOperation(Types, TT, usize),
    NullDivisionError(usize),
    UndefinedVariable(TT, usize),
    InvalidAssignment(TT, usize),
    ExpectedXFoundY(String, String, usize),
    NonFunctionReturn(usize),
    UndefinedAncestor(usize, usize),
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
            RuntimeError::UndefinedVariable(token_type, _) => {
                write!(f, "Undefined Variable : {token_type}")?;
            }
            RuntimeError::InvalidAssignment(name, _) => {
                write!(f, "Invalid Assignment: {name}")?;
            }
            RuntimeError::ExpectedXFoundY(expected, found, line) => {
                write!(f, "Expected {expected}, found {found} at line {line}")?;
            }
            RuntimeError::NonFunctionReturn(line) => {
                write!(
                    f,
                    "Returns should only be in function scopes, found at line {line}"
                )?;
            }
            RuntimeError::UndefinedAncestor(depth, line) => {
                write!(
                    f,
                    "Undefined ancestor environment at depth {depth} at line {line}"
                )?;
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
            RuntimeError::ExpectedXFoundY(_, _, line) => line,
            RuntimeError::NonFunctionReturn(line) => line,
            RuntimeError::UndefinedAncestor(_, line) => line,
        }
    }
}

type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Clone, Debug)]
pub struct Environment {
    enclosing: Option<Rc<Environment>>,
    values: RefCell<HashMap<TT, Types>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            enclosing: None,
            values: RefCell::new(HashMap::new()),
        }
    }

    pub fn global() -> Self {
        let global = Environment::new();

        global.define(
            &TT::Identifier("clock".to_string()),
            Types::Callable(Rc::new(Box::new(Clock {}))),
        );

        global
    }

    pub fn inherit_new(environment: Rc<Environment>) -> Self {
        Self {
            enclosing: Some(environment),
            values: RefCell::new(HashMap::new()),
        }
    }

    pub fn ancestor(&self, distance: usize) -> Option<Rc<Environment>> {
        let mut env = self.enclosing.as_ref().unwrap().clone();
        for _ in 1..distance {
            env = env.enclosing.as_ref()?.clone();
        }

        Some(env)
    }

    pub fn define(&self, token_type: &TT, value: Types) {
        self.values.borrow_mut().insert(token_type.clone(), value);
    }

    pub fn get(&self, token: &Token) -> RuntimeResult<Types> {
        if let Some(value) = self.values.borrow().get(&token.token_type) {
            Ok(value.clone())
        } else if let Some(ref enclosing) = self.enclosing {
            enclosing.get(token)
        } else {
            Err(RuntimeError::UndefinedVariable(
                token.token_type.clone(),
                token.line,
            ))
        }
    }
    pub fn get_at(&self, distance: usize, name_token: &Token) -> RuntimeResult<Types> {
        if distance == 0 {
            return self.get(&name_token);
        }

        if let Some(ancestor_env) = self.ancestor(distance) {
            return ancestor_env.get(name_token);
        }

        Err(RuntimeError::UndefinedAncestor(distance, name_token.line))
    }

    pub fn assign(&self, token: &Token, value: Types) -> RuntimeResult<Types> {
        if self.values.borrow().contains_key(&token.token_type) {
            self.values
                .borrow_mut()
                .insert(token.token_type.clone(), value.clone());
            Ok(value)
        } else if let Some(ref enclosing) = self.enclosing {
            enclosing.assign(token, value)
        } else {
            Err(RuntimeError::InvalidAssignment(
                token.token_type.clone(),
                token.line,
            ))
        }
    }

    pub fn assign_at(
        &self,
        distance: usize,
        name_token: &Token,
        value: Types,
    ) -> RuntimeResult<Types> {
        if distance == 0 {
            return self.assign(&name_token, value);
        }

        if let Some(ancestor_env) = self.ancestor(distance) {
            return ancestor_env.assign(name_token, value);
        }

        Err(RuntimeError::UndefinedAncestor(distance, name_token.line))
    }
}

pub struct Interpreter {
    pub global_env: Rc<Environment>,
    pub working_env: Rc<Environment>,
    had_runtime_error: bool,
    pub locals: HashMap<usize, usize>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let global_env = Rc::new(Environment::global());

        Interpreter {
            working_env: global_env.clone(),
            global_env,
            had_runtime_error: false,
            locals: HashMap::new(),
        }
    }

    pub fn resolve(&mut self, expr: &Expr, depth: usize) {
        self.locals.entry(expr.get_id()).or_insert(depth);
    }

    pub fn interpret(&mut self, program: &[Stmt]) -> (Vec<Types>, Vec<RuntimeError>) {
        let mut evals = vec![];
        let mut runtime_errors = vec![];

        for statement in program {
            match self.visit_statement(statement) {
                Ok(eval) => evals.push(eval),
                Err(runtime_exception) => match runtime_exception {
                    RuntimeReturn::Err(runtime_error) => runtime_errors.push(runtime_error),
                    RuntimeReturn::Return(runtime_token, _) => {
                        runtime_errors.push(RuntimeError::NonFunctionReturn(runtime_token.line))
                    }
                },
            }
        }

        (evals, runtime_errors)
    }
    pub fn execute_block(
        &mut self,
        statements: &[Stmt],
        environment: Environment,
    ) -> Result<Types, RuntimeReturn> {
        let previous = mem::replace(&mut self.working_env, Rc::new(environment));
        for statement in statements {
            if let Err(err) = self.visit_statement(statement) {
                if let RuntimeReturn::Return(_, _) = err {
                    self.working_env = previous;
                };

                return Err(err);
            }
        }
        self.working_env = previous;

        Ok(Types::Nil)
    }
}

impl MutVisitor for Interpreter {
    type E = Result<Types, RuntimeError>;
    type S = Result<Types, RuntimeReturn>;

    fn visit_expression(&mut self, expr: &Expr) -> Self::E {
        match expr {
            Expr::Binary(binary_expr) => {
                let right = self.visit_expression(&binary_expr.right)?;
                let left = self.visit_expression(&binary_expr.left)?;

                let error = RuntimeError::IncompatibleBinaryOperation(
                    left.clone(),
                    right.clone(),
                    binary_expr.operator.token_type.clone(),
                    binary_expr.operator.line,
                );

                match (&left, &binary_expr.operator.token_type, &right) {
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
                                    Err(RuntimeError::NullDivisionError(binary_expr.operator.line))
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
            Expr::Unary(unary_expr) => {
                let right = self.visit_expression(&unary_expr.right)?;

                match (&right, &unary_expr.operator.token_type) {
                    (Types::Number(n), &TT::Minus) => Ok(Types::Number(-n)),
                    (Types::Nil, &TT::Bang) | (Types::Boolean(false), TT::Bang) => {
                        Ok(Types::Boolean(true))
                    }
                    (_, &TT::Bang) => Ok(Types::Boolean(false)),
                    _ => Err(RuntimeError::IncompatibleUnaryOperation(
                        right,
                        unary_expr.operator.token_type.clone(),
                        unary_expr.operator.line,
                    )),
                }
            }
            Expr::Literal(literal_expr) => {
                let ret_val = match &literal_expr.literal {
                    LiteralValue::Bool(val) => Types::Boolean(*val),
                    LiteralValue::Float(val) => Types::Number(val.parse().unwrap()),
                    LiteralValue::LoxString(val) => Types::LoxString(val.clone()),
                    LiteralValue::None => Types::Nil,
                };
                Ok(ret_val)
            }
            Expr::Grouping(grouping_expr) => self.visit_expression(&grouping_expr.expression),
            Expr::Variable(variable_expr) => match self.locals.get(&variable_expr.id) {
                Some(distance) => self.working_env.get_at(*distance, &variable_expr.name),
                None => self.global_env.get(&variable_expr.name),
            },
            Expr::Assignment(assignment_expr) => {
                let value = self.visit_expression(&assignment_expr.expression)?;
                match self.locals.get(&assignment_expr.id) {
                    Some(distance) => {
                        self.working_env
                            .assign_at(*distance, &assignment_expr.name, value)
                    }
                    None => self.global_env.assign(&assignment_expr.name, value),
                }
            }
            Expr::Logical(logical_expr) => {
                let left_result = self.visit_expression(&logical_expr.left)?;

                if logical_expr.operator.token_type == TT::Or {
                    if is_truthy(&left_result) {
                        return Ok(left_result);
                    }
                } else {
                    if !is_truthy(&left_result) {
                        return Ok(left_result);
                    }
                }

                self.visit_expression(&logical_expr.right)
            }
            Expr::Call(call_expr) => {
                let callee = self.visit_expression(&call_expr.callee)?;

                let mut arguments = vec![];
                for argument in &call_expr.arguments {
                    arguments.push(self.visit_expression(argument)?);
                }

                let function = match callee {
                    Types::Callable(function) => function,
                    _ => {
                        return Err(RuntimeError::ExpectedXFoundY(
                            "Callable".to_string(),
                            callee.to_string(),
                            call_expr.closing_paren.line,
                        ))
                    }
                };

                let found_len = arguments.len();
                let expected_len = function.arity();
                if found_len != expected_len {
                    return Err(RuntimeError::ExpectedXFoundY(
                        "{expected_len} arguments".to_string(),
                        "{found_len}".to_string(),
                        call_expr.closing_paren.line,
                    ));
                }

                function.call(self, arguments)
            }
            Expr::Lambda(lambda) => {
                let lambda = Function {
                    name: None,
                    lambda: lambda.clone(),
                    closure: self.working_env.clone(),
                };

                Ok(Types::Callable(Rc::new(Box::new(lambda))))
            }
        }
    }

    fn visit_statement(&mut self, statement: &Stmt) -> Self::S {
        match statement {
            Stmt::Expression(ref expr) => {
                let evaluation = self
                    .visit_expression(expr)
                    .map_err(|err| RuntimeReturn::Err(err))?;
                return Ok(evaluation);
            }
            Stmt::Print(ref expr) => {
                let evaluation = self
                    .visit_expression(expr)
                    .map_err(|err| RuntimeReturn::Err(err))?;
                if !self.had_runtime_error {
                    println!("{evaluation}");
                }
            }
            Stmt::VariableDeclaration(ref token, ref initializer) => {
                let value = initializer
                    .as_ref()
                    .map(|expr| self.visit_expression(expr))
                    .transpose()
                    .map_err(|err| RuntimeReturn::Err(err))?
                    .unwrap_or(Types::Nil);

                self.working_env
                    .borrow_mut()
                    .define(&token.token_type, value)
            }
            Stmt::Block(ref statements) => {
                self.execute_block(
                    statements,
                    Environment::inherit_new(self.working_env.clone()),
                )?;
            }
            Stmt::While(ref condition, ref body) => {
                while is_truthy(
                    &self
                        .visit_expression(condition)
                        .map_err(|err| RuntimeReturn::Err(err))?,
                ) {
                    self.visit_statement(body)?;
                }
            }
            Stmt::If(ref condition, ref then_branch, ref else_branch) => {
                if is_truthy(
                    &self
                        .visit_expression(condition)
                        .map_err(|err| RuntimeReturn::Err(err))?,
                ) {
                    self.visit_statement(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.visit_statement(else_branch)?;
                }
            }
            Stmt::Function(ref name_token, lambda) => {
                let callable_fn = Function {
                    name: Some(name_token.token_type.clone()),
                    lambda: lambda.clone(),
                    closure: self.working_env.clone(),
                };

                self.working_env.define(
                    &name_token.token_type,
                    Types::Callable(Rc::new(Box::new(callable_fn))),
                );
            }
            Stmt::Return(keyword, value) => {
                return Err(RuntimeReturn::Return(
                    keyword.clone(),
                    match value {
                        Some(expr) => self
                            .visit_expression(expr)
                            .map_err(|err| RuntimeReturn::Err(err))?,
                        None => Types::Nil,
                    },
                ));
            }
        }

        Ok(Types::Nil)
    }
}

pub enum RuntimeReturn {
    Err(RuntimeError),
    Return(Token, Types),
}

impl fmt::Display for RuntimeReturn {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            RuntimeReturn::Err(err) => write!(f, "<Runtime Error {}>", err),
            RuntimeReturn::Return(_, return_value) => {
                write!(f, "<Runtime Return {}>", return_value)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Types {
    Number(f32),
    LoxString(String),
    Boolean(bool),
    Callable(Rc<Box<dyn Callable>>),
    Nil,
}

#[derive(Debug)]
pub struct Clock {}

#[derive(Debug)]
pub struct Function {
    name: Option<TT>,
    lambda: Rc<LambdaExpr>,
    closure: Rc<Environment>,
}

impl Function {
    fn _bind(&self, instance: Types, name: &TT) -> Function {
        let environment = Environment::global();
        environment.define(&TT::This, instance);

        Function {
            name: Some(name.clone()),
            lambda: self.lambda.clone(),
            closure: Rc::new(environment),
        }
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.lambda.parameters.len()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        mut arguments: Vec<Types>,
    ) -> RuntimeResult<Types> {
        let environment = Environment::inherit_new(self.closure.clone());

        for (i, arg) in arguments.drain(..).enumerate() {
            environment.define(&self.lambda.parameters[i].token_type, arg);
        }

        match interpreter.execute_block(&self.lambda.body, environment) {
            Ok(value) | Err(RuntimeReturn::Return(_, value)) => Ok(value),
            Err(RuntimeReturn::Err(err)) => Err(err),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "<fn {}>",
            match self.name {
                Some(ref token) => token.to_string(),
                None => "anonymous".to_string(),
            }
        )
    }
}

impl Callable for Clock {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _: &mut Interpreter, __: Vec<Types>) -> RuntimeResult<Types> {
        let time = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        Ok(Types::Number(time as f32))
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Native Clock Function")
    }
}

pub trait Callable: Debug + fmt::Display {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, _: Vec<Types>) -> RuntimeResult<Types>;
}

impl std::fmt::Display for Types {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &Types::Boolean(b) => write!(f, "{}", b),
            &Types::LoxString(ref s) => write!(f, "{}", s.to_string()),
            &Types::Nil => write!(f, "nil"),
            &Types::Number(n) => write!(f, "{}", n),
            &Types::Callable(ref c) => write!(f, "{}", c),
        }
    }
}

fn is_truthy(expression_return: &Types) -> bool {
    match expression_return {
        &Types::Nil | &Types::Boolean(false) => false,
        _ => true,
    }
}
