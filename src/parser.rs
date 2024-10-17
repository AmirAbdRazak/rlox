use std::cell::Cell;
use std::fmt;
use std::iter::Peekable;

use crate::syntax::AssignmentExpr;
use crate::{
    syntax::{BinaryExpr, Expr, Grouping, LiteralValue, Stmt, UnaryExpr, VariableExpr},
    token::{Token, TokenType as TT},
};

type BoxIterToken = Box<dyn Iterator<Item = Token>>;
type TokenPeekable = Peekable<BoxIterToken>;
type ParserResult<T> = Result<T, ParserError>;

pub enum ParserError {
    UnterminatedParentheses(usize, usize),
    NonPrimaryToken(Token),
    EmptyPrimary(usize),
    EmptyExpression(usize),
    MissingSemicolon(usize),
    ExpectedVariableName(String, usize),
    InvalidAssignmentTarget(Token, usize),
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
            ParserError::ExpectedVariableName(token, line) => {
                write!(
                    f,
                    "Parser Error: Expected variable after var at line {}, got {}.",
                    line, token
                )?;
            }
            ParserError::InvalidAssignmentTarget(token, line) => {
                write!(
                    f,
                    "Parser Error: Invalid assignment target at line {}, got {}.",
                    line, token
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
            ParserError::ExpectedVariableName(_, line) => line,
            ParserError::InvalidAssignmentTarget(_, line) => line,
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
            TT::Var => self.var_declaration(),
            _ => self.statement(),
        }
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
                _other_token => Err(ParserError::ExpectedVariableName(
                    _other_token.to_string(),
                    var_token.line,
                )),
            }
        } else {
            Err(ParserError::ExpectedVariableName(
                "None".to_string(),
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
            TT::Print => self.print_statement(),
            _ => self.expr_statement(),
        }
    }

    pub fn print_statement(&mut self) -> ParserResult<Stmt> {
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
            Some(token) => match token.token_type {
                TT::Semicolon => {
                    self.tokens.next();
                    Ok(Stmt::Expression(expr))
                }

                _ => match self.mode {
                    ParserMode::Expression => Ok(Stmt::Expression(expr)),
                    ParserMode::Statement => Err(ParserError::MissingSemicolon(token.line)),
                },
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
        let expr = self.equality()?;
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
        let intrem = match self.tokens.peek() {
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

            _ => self.primary(),
        };

        intrem
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
