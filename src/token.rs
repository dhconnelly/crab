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
    Func,
    Return,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Eq,
    EqEq,
    Less,
    Star,
    Slash,
    Plus,
    Minus,
    Comma,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub typ: TokenType,
    pub text: &'a str,
    pub line: usize,
}
