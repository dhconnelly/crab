#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    EOF,
    Ident,
    Str,
    Int,
    Let,
    Print,
    If,
    Else,
    True,
    False,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Eq,
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
