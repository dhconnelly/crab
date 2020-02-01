#[derive(Debug)]
pub enum TokenType {
    Print,
    LeftParen,
    RightParen,
    Semicolon,
    EqEq,
    Star,
    Slash,
    Plus,
    Minus,
}

#[derive(Debug)]
pub struct Token {
    typ: TokenType,
    text: String,
    line: usize,
}
