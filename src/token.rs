#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    EOF,
    Ident,
    Int,
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
pub struct Token<'a> {
    pub typ: TokenType,
    pub text: &'a str,
    pub line: usize,
}
