use std::fmt::Display;

use crate::token::Token;

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

#[derive(Debug)]
pub enum Statement {
    Let(Token, Identifier, Expression),
    Return(Token, Expression),
    Expression(Token, Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Statement::Let(_, ident, value) => write!(f, "let {} = {};", ident.value, value),
            Statement::Return(_, value) => write!(f, "return {};", value),
            Statement::Expression(_, value) => write!(f, "{};", value),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Token, String),
    Integer(Token, i64),
    Boolean(Token, bool),
    Prefix(Token, String, Box<Expression>),
    Infix(Token, Box<Expression>, String, Box<Expression>),
    If(
        Token,
        Box<Expression>,
        Vec<Statement>,
        Option<Vec<Statement>>,
    ),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Identifier(_, value) => write!(f, "{}", value),
            Expression::Integer(_, int) => write!(f, "{}", int),
            Expression::Boolean(_, boolean) => write!(f, "{}", boolean),
            Expression::Prefix(_, op, right) => write!(f, "({}{})", op, right),
            Expression::Infix(_, left, op, right) => write!(f, "({} {} {})", left, op, right),
            Expression::If(_, condition, consequence, alternative) => {
                write!(f, "if {} {{\n", condition)?;
                for stmt in consequence {
                    write!(f, "{}\n", stmt)?;
                }
                if let Some(alt) = alternative {
                    write!(f, "}} else {{\n")?;
                    for stmt in alt {
                        write!(f, "{}\n", stmt)?;
                    }
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}
