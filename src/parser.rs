use std::collections::HashMap;

use crate::{
    ast::{Expression, Identifier, Program, Statement},
    lexer::Lexer,
    token::{Keyword, Token, TokenType},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn lookup_prefix_parse_fn(
        &self,
        token_type: TokenType,
    ) -> Option<fn(&mut Parser) -> Option<Expression>> {
        match token_type {
            TokenType::Ident => Some(|p| p.parse_identifier()),
            TokenType::Int => Some(|p| p.parse_integer()),
            TokenType::Bang | TokenType::Minus => Some(|p| p.parse_prefix_expression()),
            TokenType::Keyword(Keyword::True) | TokenType::Keyword(Keyword::False) => {
                Some(|p| p.parse_boolean())
            }
            TokenType::LParen => Some(|p| p.parse_grouped_expression()),
            TokenType::Keyword(Keyword::If) => Some(|p| p.parse_if_expression()),
            _ => None,
        }
    }

    fn lookup_infix_parse_fn(
        &self,
        token_type: TokenType,
    ) -> Option<fn(&mut Parser, Expression) -> Option<Expression>> {
        match token_type {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Slash
            | TokenType::Star
            | TokenType::Eq
            | TokenType::NotEq
            | TokenType::LT
            | TokenType::GT => Some(|p, left| p.parse_infix_expression(left)),
            _ => None,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program { statements: vec![] };

        while self.current_token.is_some() {
            let statement = self.parse_statement();
            if let Some(statement) = statement {
                program.statements.push(statement);
            }
            self.next_token();
        }

        Some(program)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        // panic!("{:?}", self.current_token);
        match self.current_token?.token_type {
            TokenType::Keyword(Keyword::Let) => self.parse_let_statement(),
            TokenType::Keyword(Keyword::Return) => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn expect_peek(&mut self, token_type: TokenType) -> Option<()> {
        if self.peek_token?.token_type == token_type {
            self.next_token();
            Some(())
        } else {
            None
        }
    }

    fn expect_current(&mut self, token_type: TokenType) -> Option<()> {
        if self.current_token?.token_type == token_type {
            Some(())
        } else {
            None
        }
    }

    fn peek_precedence(&self) -> Precedence {
        if let Some(token) = self.peek_token {
            token_precedence(token)
        } else {
            Precedence::Lowest
        }
    }

    fn current_precedence(&self) -> Precedence {
        if let Some(token) = self.current_token {
            token_precedence(token)
        } else {
            Precedence::Lowest
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        assert_eq!(
            self.current_token?.token_type,
            TokenType::Keyword(Keyword::Let)
        );

        let token = self.current_token.clone()?;

        self.expect_peek(TokenType::Ident)?;

        let name_token = self.current_token.clone()?;

        let name = Identifier {
            token: name_token,
            value: self.lexer.value_of(name_token),
        };

        self.expect_peek(TokenType::Assign)?;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::Semicolon);

        Some(Statement::Let(token, name, value))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        assert_eq!(
            self.current_token?.token_type,
            TokenType::Keyword(Keyword::Return)
        );

        let token = self.current_token.clone()?;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::Semicolon);

        Some(Statement::Return(token, value))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone()?;
        let expression = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::Semicolon);
        // self.next_token();

        Some(Statement::Expression(token, expression))
    }

    fn parse_block_statement(&mut self) -> Option<Vec<Statement>> {
        let mut statements = vec![];

        // self.next_token();

        if let None = self.expect_current(TokenType::LBrace) {
            // panic!("Expected LBrace");
            println!("pbs no brace {:?}", self.current_token);
            let statement = self.parse_statement()?;
            statements.push(statement);
            return Some(statements);
        }

        // panic!("{:?}", self.current_token);
        self.next_token();

        while let Some(token) = self.current_token {
            println!("{:?}", token);
            if token.token_type == TokenType::RBrace {
                println!("pbs rbrace");
                break;
            }
            let statement = self.parse_statement()?;
            println!("{:?}", statement);
            statements.push(statement);
        }
        self.next_token();

        Some(statements)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let current_token_type = self.current_token?.token_type;
        let prefix = self.lookup_prefix_parse_fn(current_token_type)?;

        let mut left_expression = prefix(self)?;

        while self.peek_token?.token_type != TokenType::Semicolon
            && precedence < self.peek_precedence()
        {
            let infix = self.lookup_infix_parse_fn(self.peek_token?.token_type);

            if let Some(infix) = infix {
                self.next_token();

                left_expression = infix(self, left_expression)?;
            } else {
                return Some(left_expression);
            }
        }

        Some(left_expression)
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        let token = self.current_token.clone()?;

        Some(Expression::Identifier(token, self.lexer.value_of(token)))
    }

    fn parse_integer(&mut self) -> Option<Expression> {
        let token = self.current_token.clone()?;
        let value = self.lexer.value_of(token).parse().unwrap();

        Some(Expression::Integer(token, value))
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        let token = self.current_token.clone()?;
        let value = self.lexer.value_of(token) == "true";

        Some(Expression::Boolean(token, value))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        self.expect_peek(TokenType::RParen)?;
        // self.next_token();

        expression
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let token = self.current_token.clone()?;
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;
        println!("{:?}", condition);

        self.expect_peek(TokenType::RParen);

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token?.token_type == TokenType::Keyword(Keyword::Else) {
            self.next_token();
            self.parse_block_statement()
        } else {
            None
        };

        Some(Expression::If(
            token,
            Box::new(condition),
            consequence,
            alternative,
        ))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.current_token.clone()?;
        let operator = self.lexer.value_of(token);

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Some(Expression::Prefix(token, operator, Box::new(right)))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let precedence = self.current_precedence();
        let token = self.current_token.clone()?;
        self.next_token();

        let right = self.parse_expression(precedence)?;

        Some(Expression::Infix(
            token,
            Box::new(left),
            self.lexer.value_of(token),
            Box::new(right),
        ))
    }
}

fn token_precedence(token: Token) -> Precedence {
    match token.token_type {
        TokenType::Eq => Precedence::Equals,
        TokenType::NotEq => Precedence::Equals,
        TokenType::LT => Precedence::LessGreater,
        TokenType::GT => Precedence::LessGreater,
        TokenType::Plus => Precedence::Sum,
        TokenType::Minus => Precedence::Sum,
        TokenType::Slash => Precedence::Product,
        TokenType::Star => Precedence::Product,
        _ => Precedence::Lowest,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_parsing() {
        let mut lexer = Lexer::new("let x = 5; let y = x;");
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program().unwrap();

        assert_eq!(program.statements.len(), 2);

        for statement in program.statements {
            println!("{statement}");
        }
    }

    #[test]
    fn test_return_parsing() {
        let mut lexer = Lexer::new("return foobar; return 5;");
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program().unwrap();

        assert_eq!(program.statements.len(), 2);

        for statement in program.statements {
            println!("{statement}");
        }
    }

    #[test]
    fn test_expression_statement_parsing() {
        let mut lexer = Lexer::new("foobar; 5;");
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program().unwrap();

        assert_eq!(program.statements.len(), 2);

        for statement in program.statements {
            println!("{:?}", statement);
        }
    }

    #[test]
    fn test_prefix_expression_parsing() {
        let mut lexer = Lexer::new("-5; !isTrue;");
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program().unwrap();

        assert_eq!(program.statements.len(), 2);

        for statement in program.statements {
            println!("{statement}");
        }
    }

    #[test]
    fn test_infix_expression_parsing() {
        let mut lexer = Lexer::new("a + b * c; 2 * 3 + 4;");
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program().unwrap();

        assert_eq!(program.statements.len(), 2);

        for statement in program.statements {
            println!("{statement}");
        }
    }

    #[test]
    fn test_bool_expression_parsing() {
        let mut lexer = Lexer::new("let x = true; let y = false;");
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program().unwrap();

        assert_eq!(program.statements.len(), 2);

        for statement in program.statements {
            println!("{statement}");
        }
    }

    #[test]
    fn test_grouped_expression_parsing() {
        let mut lexer = Lexer::new("(5 + 5) * 2; (4 + 4) * 3;");
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program().unwrap();

        assert_eq!(program.statements.len(), 2);

        for statement in program.statements {
            println!("{statement}\n{statement:?}");
        }
    }

    #[test]
    fn test_if_expression_parsing() {
        let mut lexer = Lexer::new("if (x < y) { return true; } else { return false; }");
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program().unwrap();

        let len = program.statements.len();

        for statement in program.statements {
            println!("{statement}\n{statement:?}");
        }
        assert_eq!(len, 1);
    }
}
