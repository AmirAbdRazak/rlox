use std::cell::Cell;
use std::fmt;
use std::iter::Peekable;
use std::rc::Rc;

use crate::syntax::{AssignmentExpr, CallExpr, LambdaExpr, LogicalExpr};
use crate::{
    syntax::{BinaryExpr, Expr, Grouping, LiteralValue, Stmt, UnaryExpr, VariableExpr},
    token::{Token, TokenType as TT},
};

type BoxIterToken = Box<dyn Iterator<Item = Token>>;
type TokenPeekable = Peekable<BoxIterToken>;
type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    UnterminatedParentheses(usize, usize),
    NonPrimaryToken(Token),
    EmptyPrimary(usize),
    EmptyExpression(usize),
    MissingSemicolon(usize),
    InvalidAssignmentTarget(Token, usize),
    MissingClosingBraces(usize),
    ExpectedXAfterY(TT, String, usize),
    MaxCallArguments(String, usize),
}

pub enum FunctionKind {
    Function,
}

impl fmt::Display for FunctionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Function => {
                write!(f, "Function")
            }
        }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::UnterminatedParentheses(r_line, l_line) => {
                write!(f, "Parser Error: Expecting terminating parentheses at line {}, unterminated parentheses located at line {}", r_line, l_line)?;
            }
            ParserError::NonPrimaryToken(token) => {
                write!(
                    f,
                    "Parser Error: Unsupported token {:?} at line {}",
                    token.token_type, token.line
                )?;
            }
            ParserError::EmptyPrimary(line) => {
                write!(
                    f,
                    "Parser Error: Expecting a token here at line {}, none found.",
                    line
                )?;
            }
            ParserError::EmptyExpression(line) => {
                write!(
                    f,
                    "Parser Error: Empty expressions are illegal, found at line {}",
                    line
                )?;
            }
            ParserError::MissingSemicolon(line) => {
                write!(f, "Parser Error: Expected semicolon at line {}.", line)?;
            }
            ParserError::MaxCallArguments(name, line) => write!(
                f,
                "Parser Error: Max call arguments achieved on {} at line {}",
                name, line
            )?,
            ParserError::InvalidAssignmentTarget(token, line) => {
                write!(
                    f,
                    "Parser Error: Invalid assignment target at line {}, got {}.",
                    line, token
                )?;
            }
            ParserError::MissingClosingBraces(line) => {
                write!(f, "Parser Error: Missing closing braces on line {}.", line)?;
            }
            ParserError::ExpectedXAfterY(expected_token_type, term, line) => {
                write!(
                    f,
                    "Parser Error: Expected {} after {} on line {}.",
                    expected_token_type, term, line
                )?;
            }
        }

        Ok(())
    }
}

impl ParserError {
    pub fn line(&self) -> usize {
        match *self {
            ParserError::UnterminatedParentheses(line, _) => line,
            ParserError::NonPrimaryToken(Token { line, .. }) => line,
            ParserError::EmptyPrimary(line) => line,
            ParserError::EmptyExpression(line) => line,
            ParserError::MissingSemicolon(line) => line,
            ParserError::InvalidAssignmentTarget(_, line) => line,
            ParserError::MissingClosingBraces(line) => line,
            ParserError::ExpectedXAfterY(_, _, line) => line,
            ParserError::MaxCallArguments(_, line) => line,
        }
    }
}

pub enum ParserMode {
    Expression,
    Statement,
}

pub struct Parser {
    tokens: TokenPeekable,
    prev_token_line: usize,
    mode: ParserMode,
    id: Cell<usize>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, mode: ParserMode) -> Parser {
        let iter_tokens: BoxIterToken = Box::new(tokens.into_iter());
        Parser {
            tokens: iter_tokens.peekable(),
            prev_token_line: 0,
            mode,
            id: Cell::new(0),
        }
    }
    fn new_id(&self) -> usize {
        let new_id = self.id.get();
        self.id.set(new_id + 1);
        new_id
    }

    pub fn parse(&mut self) -> (Vec<Stmt>, Vec<ParserError>) {
        let mut statements: Vec<Stmt> = vec![];
        let mut errs: Vec<ParserError> = vec![];

        while let Some(peek_token) = self.tokens.peek() {
            if peek_token.token_type == TT::Eof {
                self.tokens.next();
                continue;
            }

            match self.declaration() {
                Ok(statement) => statements.push(statement),
                Err(err) => {
                    self.synchronize();
                    errs.push(err);
                }
            }
        }
        (statements, errs)
    }

    pub fn declaration(&mut self) -> ParserResult<Stmt> {
        match self
            .tokens
            .peek()
            .expect("Already checked above but for whatever reason if this gets up then idk")
            .token_type
        {
            TT::Fun => self.function_declaration(FunctionKind::Function),
            TT::Var => self.var_declaration(),
            _ => self.statement(),
        }
    }

    pub fn function_declaration(&mut self, function_kind: FunctionKind) -> ParserResult<Stmt> {
        assert_eq!(self.tokens.peek().unwrap().token_type, TT::Fun);
        let fun_token = self.tokens.next().unwrap();

        let name = match self.tokens.peek() {
            Some(
                _token @ Token {
                    token_type: TT::Identifier(_),
                    ..
                },
            ) => self.tokens.next().unwrap(),
            _ => {
                return Err(ParserError::ExpectedXAfterY(
                    TT::Identifier("".to_string()),
                    "function declaration".to_string(),
                    fun_token.line,
                ));
            }
        };

        let body = self.function_body(function_kind, Some(&name), false)?;
        Ok(Stmt::Function(name, Rc::new(body)))
    }

    pub fn function_body(
        &mut self,
        function_kind: FunctionKind,
        name: Option<&Token>,
        is_lambda: bool,
    ) -> ParserResult<LambdaExpr> {
        let left_hand = if is_lambda { TT::Pipe } else { TT::LeftParen };
        let right_hand = if is_lambda { TT::Pipe } else { TT::RightParen };

        let left_hand_token = match self.tokens.peek() {
            Some(token) if token.token_type == left_hand => self.tokens.next().unwrap(),
            _ => {
                return Err(ParserError::ExpectedXAfterY(
                    left_hand,
                    format!("{function_kind} name"),
                    self.prev_token_line,
                ));
            }
        };

        let mut parameters: Vec<Token> = Vec::new();
        if matches!(self.tokens.peek(), Some(token) if token.token_type != right_hand) {
            loop {
                if parameters.len() >= 8 {
                    return Err(match name {
                        Some(token) => ParserError::MaxCallArguments(token.to_string(), token.line),
                        None => ParserError::MaxCallArguments(
                            "anonymous function".to_string(),
                            left_hand_token.line,
                        ),
                    });
                }

                match self.tokens.peek() {
                    Some(token) if matches!(token.token_type, TT::Identifier(_)) => {
                        parameters.push(self.tokens.next().unwrap().clone());
                    }
                    _ => {
                        return Err(ParserError::ExpectedXAfterY(
                            TT::Identifier("".to_string()),
                            "arguments".to_string(),
                            left_hand_token.line,
                        ))
                    }
                };

                match self.tokens.peek() {
                    Some(token) if token.token_type == TT::Comma => {
                        self.tokens.next();
                    }
                    _ => break,
                }
            }
        }

        match self.tokens.peek() {
            Some(token) if token.token_type == right_hand => self.tokens.next(),
            _ => {
                return Err(ParserError::ExpectedXAfterY(
                    right_hand,
                    "function arguments".to_string(),
                    left_hand_token.line,
                ));
            }
        };

        let body = match self.block_statement()? {
            Stmt::Block(block_statements) => block_statements,
            _ => unreachable!(),
        };

        Ok(LambdaExpr { parameters, body })
    }

    pub fn var_declaration(&mut self) -> ParserResult<Stmt> {
        let var_token = self
            .tokens
            .next()
            .expect("It just said there was a Var in the previous peek");

        if let Some(_) = self.tokens.peek() {
            let token = self.tokens.next().unwrap();
            match token.token_type {
                TT::Identifier(_) => {
                    let expr =
                        if self.tokens.peek().is_some_and(|next_token| {
                            matches!(next_token.token_type, TT::Assignment)
                        }) {
                            let _consume_eq = self.tokens.next().unwrap();
                            Some(self.expression()?)
                        } else {
                            None
                        };

                    match self.tokens.next().unwrap().token_type {
                        TT::Semicolon => Ok(Stmt::VariableDeclaration(token.clone(), expr)),
                        _ => Err(ParserError::MissingSemicolon(token.line)),
                    }
                }
                _other_token => Err(ParserError::ExpectedXAfterY(
                    TT::Identifier("Identifier".to_string()),
                    var_token.to_string(),
                    var_token.line,
                )),
            }
        } else {
            Err(ParserError::ExpectedXAfterY(
                TT::Identifier("Identifier".to_string()),
                var_token.to_string(),
                var_token.line,
            ))
        }
    }
    pub fn statement(&mut self) -> ParserResult<Stmt> {
        match self
            .tokens
            .peek()
            .expect("Already checked above but for whatever reason if this gets up then idk")
            .token_type
        {
            TT::If => self.if_statement(),
            TT::Print => self.print_statement(),
            TT::LeftBrace => self.block_statement(),
            TT::While => self.while_statement(),
            TT::For => self.for_statement(),
            TT::Return => self.return_statement(),
            _ => self.expr_statement(),
        }
    }

    pub fn return_statement(&mut self) -> ParserResult<Stmt> {
        let return_keyword = self.tokens.next().unwrap();

        let value = match self.tokens.peek() {
            Some(token) if token.token_type == TT::Semicolon => {
                self.tokens.next();
                None
            }
            _ => {
                let expr = self.expression()?;

                match self.tokens.peek() {
                    Some(token) if token.token_type == TT::Semicolon => {
                        self.tokens.next();
                    }
                    _ => return Err(ParserError::MissingSemicolon(return_keyword.line)),
                };

                Some(expr)
            }
        };

        Ok(Stmt::Return(return_keyword, value))
    }

    pub fn for_statement(&mut self) -> ParserResult<Stmt> {
        assert_eq!(self.tokens.peek().unwrap().token_type, TT::For);
        let for_token = self.tokens.next().unwrap();

        match self.tokens.peek() {
            Some(token) if token.token_type == TT::LeftParen => self.tokens.next(),
            _ => {
                return Err(ParserError::ExpectedXAfterY(
                    TT::LeftBrace,
                    for_token.to_string(),
                    for_token.line,
                ))
            }
        };

        let initializer = match self.tokens.peek() {
            Some(token) if token.token_type == TT::Semicolon => None,
            Some(token) if token.token_type == TT::Var => Some(self.var_declaration()?),
            _ => Some(self.expr_statement()?),
        };

        let condition = match self.tokens.peek() {
            Some(token) if token.token_type == TT::Semicolon => None,
            _ => Some(self.expression()?),
        };

        match self.tokens.peek() {
            Some(token) if token.token_type == TT::Semicolon => self.tokens.next(),
            _ => {
                return Err(ParserError::ExpectedXAfterY(
                    TT::Semicolon,
                    "For loop condition".to_string(),
                    for_token.line,
                ))
            }
        };

        let increment = match self.tokens.peek() {
            Some(token) if token.token_type == TT::RightParen => None,
            _ => Some(self.expression()?),
        };

        match self.tokens.peek() {
            Some(token) if token.token_type == TT::RightParen => self.tokens.next(),
            _ => {
                return Err(ParserError::ExpectedXAfterY(
                    TT::Semicolon,
                    "For loop increment".to_string(),
                    for_token.line,
                ))
            }
        };

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(increment)]);
        }

        body = match condition {
            Some(condition_inner) => Stmt::While(condition_inner, Box::new(body)),
            None => Stmt::While(Expr::Literal(LiteralValue::Bool(true)), Box::new(body)),
        };

        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
        }

        Ok(body)
    }

    pub fn while_statement(&mut self) -> ParserResult<Stmt> {
        assert_eq!(self.tokens.peek().unwrap().token_type, TT::While);
        let while_token = self.tokens.next().unwrap();

        match self.tokens.peek() {
            Some(token) if token.token_type == TT::LeftBrace => self.tokens.next(),
            _ => {
                return Err(ParserError::ExpectedXAfterY(
                    TT::LeftBrace,
                    while_token.to_string(),
                    while_token.line,
                ))
            }
        };

        let condition = self.expression()?;
        match self.tokens.peek() {
            Some(token) if token.token_type == TT::RightBrace => self.tokens.next(),
            _ => {
                return Err(ParserError::ExpectedXAfterY(
                    TT::RightBrace,
                    "While token condition".to_string(),
                    while_token.line,
                ))
            }
        };

        let body = self.statement()?;

        Ok(Stmt::While(condition, Box::new(body)))
    }

    pub fn if_statement(&mut self) -> ParserResult<Stmt> {
        assert_eq!(self.tokens.peek().unwrap().token_type, TT::If);
        let if_token = self.tokens.next().unwrap();

        match self.tokens.peek() {
            Some(token) if token.token_type == TT::LeftParen => self.tokens.next(),
            _ => {
                return Err(ParserError::ExpectedXAfterY(
                    TT::LeftParen,
                    if_token.to_string(),
                    if_token.line,
                ))
            }
        };

        let condition = self.expression()?;
        match self.tokens.peek() {
            Some(token) if token.token_type == TT::RightParen => self.tokens.next(),
            _ => {
                return Err(ParserError::ExpectedXAfterY(
                    TT::RightParen,
                    "If condition".to_string(),
                    if_token.line,
                ))
            }
        };

        let then_branch = Box::new(self.statement()?);
        let else_branch = match self.tokens.peek() {
            Some(token) if token.token_type == TT::Else => {
                self.tokens.next();
                Some(Box::new(self.statement()?))
            }
            _ => None,
        };

        Ok(Stmt::If(condition, then_branch, else_branch))
    }

    pub fn block_statement(&mut self) -> ParserResult<Stmt> {
        assert_eq!(self.tokens.peek().unwrap().token_type, TT::LeftBrace);

        let mut statements: Vec<Stmt> = Vec::new();
        let _consume_left_brace = self.tokens.next();

        while match self.tokens.peek() {
            Some(token) if matches!(token.token_type, TT::RightBrace) => false,
            _ => true,
        } {
            let decl = self.declaration()?;
            statements.push(decl);
        }

        match self.tokens.next() {
            Some(token) if matches!(token.token_type, TT::RightBrace) => {
                Ok(Stmt::Block(statements))
            }
            _ => Err(ParserError::MissingClosingBraces(self.prev_token_line)),
        }
    }

    pub fn print_statement(&mut self) -> ParserResult<Stmt> {
        assert_eq!(self.tokens.peek().unwrap().token_type, TT::Print);
        let _print_token = self.tokens.next().unwrap();
        let value = self.expression()?;

        match self.tokens.peek() {
            Some(token) if token.token_type == TT::Semicolon => self.tokens.next(),
            Some(token) => return Err(ParserError::MissingSemicolon(token.line)),
            _ => return Err(ParserError::MissingSemicolon(self.prev_token_line)),
        };

        Ok(Stmt::Print(value))
    }

    pub fn expr_statement(&mut self) -> ParserResult<Stmt> {
        let expr = self.expression()?;

        match self.tokens.peek() {
            Some(token) if token.token_type == TT::Semicolon => {
                self.tokens.next();
                Ok(Stmt::Expression(expr))
            }
            Some(token) => match self.mode {
                ParserMode::Expression => Ok(Stmt::Expression(expr)),
                ParserMode::Statement => Err(ParserError::MissingSemicolon(token.line)),
            },
            _ => match self.mode {
                ParserMode::Expression => Ok(Stmt::Expression(expr)),
                ParserMode::Statement => Err(ParserError::MissingSemicolon(self.prev_token_line)),
            },
        }
    }

    fn binary_expr_generator(
        &mut self,
        expr_fn: Box<dyn Fn(&mut Parser) -> ParserResult<Expr>>,
        expr_tokens: &[TT],
    ) -> ParserResult<Expr> {
        let mut expr = expr_fn(self)?;

        while let Some(token) = self.tokens.peek() {
            if expr_tokens.contains(&token.token_type) {
                // Update prev token.line pointer
                self.prev_token_line = token.line;

                let operator = self.tokens.next().unwrap();
                let right = expr_fn(self)?;
                expr = Expr::Binary(BinaryExpr {
                    left: Box::new(expr),
                    right: Box::new(right),
                    operator,
                })
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn expression(&mut self) -> ParserResult<Expr> {
        return self.assignment();
    }

    fn assignment(&mut self) -> ParserResult<Expr> {
        let expr = self.or()?;
        let peek_token = self.tokens.peek().unwrap();

        if matches!(peek_token.token_type, TT::Assignment) {
            // Update prev token.line pointer
            let line = peek_token.line;
            self.prev_token_line = line;
            let operator = self.tokens.next().unwrap();
            let right = self.assignment()?;

            match expr {
                Expr::Variable(VariableExpr { id: _id, ref name }) => {
                    return Ok(Expr::Assignment(AssignmentExpr {
                        id: self.new_id(),
                        name: name.clone(),
                        expression: Box::new(right),
                    }));
                }
                _ => return Err(ParserError::InvalidAssignmentTarget(operator.clone(), line)),
            }
        }

        Ok(expr)
    }

    fn or(&mut self) -> ParserResult<Expr> {
        let mut expr = self.and()?;
        while let Some(token) = self.tokens.peek() {
            if token.token_type == TT::Or {
                self.prev_token_line = token.line;

                let operator = self.tokens.next().unwrap();
                let right = self.and()?;

                expr = Expr::Logical(LogicalExpr {
                    left: Box::new(expr),
                    right: Box::new(right),
                    operator,
                })
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn and(&mut self) -> ParserResult<Expr> {
        let mut expr = self.equality()?;
        while let Some(token) = self.tokens.peek() {
            if token.token_type == TT::And {
                self.prev_token_line = token.line;

                let operator = self.tokens.next().unwrap();
                let right = self.equality()?;

                expr = Expr::Logical(LogicalExpr {
                    left: Box::new(expr),
                    right: Box::new(right),
                    operator,
                })
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> ParserResult<Expr> {
        let equality_tokens = [TT::BangEqual, TT::EqualEqual];
        self.binary_expr_generator(Box::new(Parser::comparison), &equality_tokens)
    }

    fn comparison(&mut self) -> ParserResult<Expr> {
        let comparison_tokens = [TT::Greater, TT::GreaterEqual, TT::Less, TT::LessEqual];
        self.binary_expr_generator(Box::new(Parser::term), &comparison_tokens)
    }

    fn term(&mut self) -> ParserResult<Expr> {
        let term_tokens = [TT::Minus, TT::Plus];
        self.binary_expr_generator(Box::new(Parser::factor), &term_tokens)
    }

    fn factor(&mut self) -> ParserResult<Expr> {
        let factor_tokens = [TT::Slash, TT::Star];
        self.binary_expr_generator(Box::new(Parser::unary), &factor_tokens)
    }

    fn unary(&mut self) -> ParserResult<Expr> {
        let unary_tokens = [TT::Bang, TT::Minus];
        match self.tokens.peek() {
            Some(token) if unary_tokens.contains(&token.token_type) => {
                // Update prev token.line pointer
                self.prev_token_line = token.line;

                let operator = self.tokens.next().unwrap();
                let right = self.unary()?;
                Ok(Expr::Unary(UnaryExpr {
                    right: Box::new(right),
                    operator,
                }))
            }

            _ => self.call(),
        }
    }

    fn call(&mut self) -> ParserResult<Expr> {
        let mut expr = self.primary()?;

        loop {
            match self.tokens.peek() {
                Some(token) if token.token_type == TT::LeftParen => {
                    self.prev_token_line = token.line;

                    let _consume_left_paren = self.tokens.next();
                    expr = self.finish_call(expr)?;
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> ParserResult<Expr> {
        let mut arguments = vec![];
        if matches!(self.tokens.peek(), Some(token) if token.token_type != TT::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    return Err(ParserError::MaxCallArguments(
                        callee.to_string(),
                        self.prev_token_line,
                    ));
                }

                arguments.push(Box::new(self.expression()?));

                match self.tokens.peek() {
                    Some(token) if token.token_type == TT::Comma => self.tokens.next(),
                    _ => break,
                };
            }
        }

        let closing_paren = match self.tokens.next() {
            Some(token) if token.token_type == TT::RightParen => token,
            Some(token) => {
                return Err(ParserError::ExpectedXAfterY(
                    TT::RightParen,
                    "function arguments".to_string(),
                    token.line,
                ))
            }
            _ => {
                return Err(ParserError::ExpectedXAfterY(
                    TT::RightParen,
                    "function_arguments".to_string(),
                    self.prev_token_line,
                ))
            }
        };

        Ok(Expr::Call(CallExpr {
            callee: Box::new(callee),
            closing_paren,
            arguments,
        }))
    }

    fn lambda(&mut self) -> ParserResult<Expr> {
        assert_eq!(self.tokens.peek().unwrap().token_type, TT::Pipe);

        Ok(Expr::Lambda(Rc::new(self.function_body(
            FunctionKind::Function,
            None,
            true,
        )?)))
    }

    fn primary(&mut self) -> ParserResult<Expr> {
        if let Some(peek_token) = self.tokens.peek() {
            self.prev_token_line = peek_token.line;

            match &peek_token.token_type {
                // Handle parentheses scoping
                TT::LeftParen => {
                    let token = self.tokens.next().unwrap();
                    let result_expr = self.expression();

                    let expr = match result_expr {
                        Ok(ex) => Ok(ex),

                        Err(ParserError::NonPrimaryToken(token)) => match token.token_type {
                            TT::RightParen => Err(ParserError::EmptyExpression(token.line)),
                            _ => Err(ParserError::NonPrimaryToken(token)),
                        },
                        Err(err) => Err(err),
                    }?;

                    match self
                        .tokens
                        .next()
                        .ok_or(ParserError::UnterminatedParentheses(
                            self.prev_token_line,
                            token.line,
                        ))?
                        .token_type
                    {
                        TT::RightParen => Ok(Expr::Grouping(Grouping {
                            expression: Box::new(expr),
                        })),
                        _token => Err(ParserError::UnterminatedParentheses(
                            self.prev_token_line,
                            token.line,
                        )),
                    }
                }

                // Handle literals
                TT::False => self.consume_and_cast_literal(false.into()),
                TT::True => self.consume_and_cast_literal(true.into()),
                TT::Nil => self.consume_and_cast_literal(LiteralValue::None),
                TT::Number(borrowed_float) => {
                    let float = borrowed_float.clone();
                    self.consume_and_cast_literal(LiteralValue::Float(float))
                }
                TT::LoxString(borrowed_str) => {
                    let lox_string = borrowed_str.clone();
                    self.consume_and_cast_literal(lox_string.into())
                }
                TT::Pipe => self.lambda(),
                TT::Identifier(_) => {
                    let identifier_token = self.tokens.next().unwrap();
                    let new_id = self.new_id();
                    Ok(Expr::Variable(VariableExpr {
                        id: new_id,
                        name: identifier_token,
                    }))
                }

                _ => Err(ParserError::NonPrimaryToken(
                    self.tokens.next().expect("at non primary token error"),
                )),
            }
        } else {
            Err(ParserError::EmptyPrimary(self.prev_token_line))
        }
    }

    fn consume_and_cast_literal(&mut self, literal_value: LiteralValue) -> ParserResult<Expr> {
        self.tokens.next();
        Ok(Expr::Literal(literal_value))
    }

    fn synchronize(&mut self) {
        self.tokens.next();

        while let Some(token) = self.tokens.peek() {
            if [
                TT::Class,
                TT::Fun,
                TT::Var,
                TT::For,
                TT::If,
                TT::While,
                TT::Return,
                TT::Print,
            ]
            .contains(&token.token_type)
            {
                return;
            }

            self.tokens.next();
        }
    }
}
