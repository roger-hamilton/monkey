use crate::token::{Keyword, Token, TokenType};

pub struct Lexer {
    input: Vec<char>,
    offset: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            offset: 0,
        }
    }

    pub(crate) fn value_of(&self, token: Token) -> String {
        self.input[token.start..token.start + token.len]
            .iter()
            .collect()
    }

    fn line_at(&self, offset: usize) -> usize {
        self.input[..offset].iter().filter(|&&c| c == '\n').count() + 1
    }

    fn column_at(&self, offset: usize) -> usize {
        self.input[..offset]
            .iter()
            .rev()
            .take_while(|&&c| c != '\n')
            .count()
            + 1
    }

    fn loc_at(&self, offset: usize) -> (usize, usize) {
        (self.line_at(offset), self.column_at(offset))
    }

    pub(crate) fn loc_of(&self, token: Token) -> (usize, usize) {
        self.loc_at(token.start)
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.offset).cloned()
    }

    fn next(&mut self) -> Option<char> {
        let c = self.peek();
        self.offset += 1;
        c
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.offset += 1;
            } else {
                break;
            }
        }
    }

    fn simple_token(&self, token_type: TokenType) -> Option<Token> {
        let start = self.offset - 1;
        Some(Token {
            token_type,
            start,
            len: 1,
            // lexer: self,
        })
    }

    fn keyword_token(&self, keyword: Keyword, len: usize) -> Option<Token> {
        Some(Token {
            token_type: TokenType::Keyword(keyword),
            start: self.offset - len,
            len,
            // lexer: self,
        })
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        let c = self.next()?;

        match c {
            '+' => self.simple_token(TokenType::Plus),
            '-' => self.simple_token(TokenType::Minus),
            '*' => self.simple_token(TokenType::Star),
            '/' => self.simple_token(TokenType::Slash),
            ',' => self.simple_token(TokenType::Comma),
            ';' => self.simple_token(TokenType::Semicolon),
            '<' => self.simple_token(TokenType::LT),
            '>' => self.simple_token(TokenType::GT),
            '[' => self.simple_token(TokenType::LBracket),
            ']' => self.simple_token(TokenType::RBracket),
            '(' => self.simple_token(TokenType::LParen),
            ')' => self.simple_token(TokenType::RParen),
            '{' => self.simple_token(TokenType::LBrace),
            '}' => self.simple_token(TokenType::RBrace),
            '=' => {
                if self.peek() == Some('=') {
                    self.next();
                    Some(Token {
                        token_type: TokenType::Eq,
                        start: self.offset - 2,
                        len: 2,
                        // lexer: self,
                    })
                } else {
                    self.simple_token(TokenType::Assign)
                }
            }
            '!' => {
                if self.peek() == Some('=') {
                    self.next();
                    Some(Token {
                        token_type: TokenType::NotEq,
                        start: self.offset - 2,
                        len: 2,
                        // lexer: self,
                    })
                } else {
                    self.simple_token(TokenType::Bang)
                }
            }
            _ if c.is_numeric() => {
                let start = self.offset - 1;
                while let Some(c) = self.peek() {
                    if c.is_numeric() {
                        self.offset += 1;
                    } else {
                        break;
                    }
                }
                Some(Token {
                    token_type: TokenType::Int,
                    start,
                    len: self.offset - start,
                    // lexer: self,
                })
            }
            _ if c.is_alphabetic() => {
                let start = self.offset - 1;
                while let Some(c) = self.peek() {
                    if c.is_alphanumeric() {
                        self.offset += 1;
                    } else {
                        break;
                    }
                }

                let ident = Token {
                    token_type: TokenType::Ident,
                    start,
                    len: self.offset - start,
                    // lexer: self,
                };

                match self.value_of(ident).as_str() {
                    "let" => self.keyword_token(Keyword::Let, 3),
                    "fn" => self.keyword_token(Keyword::Fn, 2),
                    "if" => self.keyword_token(Keyword::If, 2),
                    "else" => self.keyword_token(Keyword::Else, 4),
                    "return" => self.keyword_token(Keyword::Return, 6),
                    "true" => self.keyword_token(Keyword::True, 4),
                    "false" => self.keyword_token(Keyword::False, 5),
                    _ => Some(ident),
                }
            }

            _ => self.simple_token(TokenType::Invalid),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);

!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;"#;

        let mut parser = Lexer::new(input);

        while let Some(token) = parser.next_token() {
            assert_ne!(token.token_type, TokenType::Invalid);
        }
    }
}
