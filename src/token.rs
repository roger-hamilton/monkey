use crate::lexer::Lexer;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenType {
    Ident,
    Int,

    Plus,
    Minus,
    Star,
    Slash,

    GT,
    LT,

    Assign,
    Eq,
    NotEq,
    Bang,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Keyword(Keyword),

    Invalid,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Keyword {
    Let,
    Fn,
    If,
    Else,
    Return,
    True,
    False,
}

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub start: usize,
    pub len: usize,
    // pub(crate) lexer: &'a Lexer,
}

// impl<'a> std::fmt::Debug for Token<'a> {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         let value = self.lexer.value_of(*self);
//         let loc = self.lexer.loc_of(*self);
//         write!(
//             f,
//             "Token {{ type: {:?}, value: {:?}, loc: {:?} }}",
//             self.token_type, value, loc
//         )
//     }
// }
