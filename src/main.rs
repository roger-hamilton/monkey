use lexer::Lexer;

mod ast;
mod lexer;
mod parser;
mod token;

fn main() {
    let mut lexer = Lexer::new("let x = 5;");
    while let Some(token) = lexer.next_token() {
        println!("{:?}", token);
    }
}
