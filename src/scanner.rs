use crate::token::{Token, TokenType, TokenType::*};
use std::error;
use std::fmt;
use std::iter::Peekable;
use std::result;
use std::str::CharIndices;

#[derive(Debug, Clone, Copy)]
pub enum ScanErrType {
    BadToken(char),
    UnterminatedString,
}
use ScanErrType::*;

#[derive(Debug, Clone, Copy)]
pub struct ScanError {
    pub typ: ScanErrType,
    pub line: usize,
}

impl error::Error for ScanError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = match &self.typ {
            BadToken(x) => format!("bad token: {}", x),
            UnterminatedString => format!("unterminated string"),
        };
        write!(f, "scanner: line {}: {}", self.line, msg)
    }
}

type Result<T> = result::Result<T, ScanError>;

struct Scanner<'a> {
    text: &'a str,
    chars: Peekable<CharIndices<'a>>,
    line: usize,
}

impl<'a> Scanner<'a> {
    fn new(text: &'a str) -> Scanner<'a> {
        let chars = text.char_indices().peekable();
        let line = 1;
        Scanner { text, chars, line }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, x)| *x)
    }

    fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn peek_is(&mut self, ch: char) -> bool {
        match self.peek() {
            None => false,
            Some(x) => x == ch,
        }
    }

    fn err(&self, typ: ScanErrType) -> ScanError {
        let line = self.line;
        ScanError { line, typ }
    }

    fn emit(&self, typ: TokenType, text: &'a str) -> Token<'a> {
        let line = self.line;
        Token { typ, text, line }
    }

    fn emit1(&self, typ: TokenType, i: usize) -> Token<'a> {
        self.emit(typ, &self.text[i..i + 1])
    }

    fn eat_while(&mut self, from: usize, f: impl Fn(char) -> bool) -> &'a str {
        while !self.at_end() && f(self.peek().unwrap()) {
            self.chars.next().unwrap();
        }
        let until = match self.chars.peek() {
            None => self.text.len(),
            Some((i, _)) => *i,
        };
        &self.text[from..until]
    }

    fn ident(&mut self, from: usize) -> Token<'a> {
        let text = self.eat_while(from, |ch| ch.is_alphanumeric());
        let typ = match text {
            "print" => Print,
            "true" => True,
            "false" => False,
            "if" => If,
            "else" => Else,
            "let" => Let,
            "fn" => Func,
            "return" => Return,
            "loop" => Loop,
            "break" => Break,
            _ => Ident,
        };
        self.emit(typ, text)
    }

    fn int(&mut self, from: usize) -> Token<'a> {
        let text = self.eat_while(from, |ch| ch.is_numeric());
        self.emit(Int, text)
    }

    fn eq(&mut self, from: usize) -> Token<'a> {
        self.chars.next();
        self.emit(EqEq, &self.text[from..from + 2])
    }

    fn comment(&mut self, from: usize) {
        self.eat_while(from, |ch| ch != '\n');
    }

    fn str(&mut self, from: usize) -> Result<Token<'a>> {
        let text = self.eat_while(from, |ch| ch != '"');
        if self.peek_is('"') {
            self.chars.next();
            Ok(self.emit(Str, &text[1..]))
        } else {
            Err(self.err(UnterminatedString))
        }
    }

    fn scan(&mut self) -> Result<Token<'a>> {
        loop {
            if self.at_end() {
                return Ok(self.emit(EOF, ""));
            }
            let (i, ch) = self.chars.next().unwrap();
            match ch {
                '\n' => self.line += 1,
                '(' => return Ok(self.emit1(LeftParen, i)),
                ')' => return Ok(self.emit1(RightParen, i)),
                '{' => return Ok(self.emit1(LeftBrace, i)),
                '}' => return Ok(self.emit1(RightBrace, i)),
                ';' => return Ok(self.emit1(Semicolon, i)),
                '=' if self.peek_is('=') => return Ok(self.eq(i)),
                '=' => return Ok(self.emit1(Eq, i)),
                '*' => return Ok(self.emit1(Star, i)),
                '/' if self.peek_is('/') => self.comment(i),
                '/' => return Ok(self.emit1(Slash, i)),
                '+' => return Ok(self.emit1(Plus, i)),
                '-' => return Ok(self.emit1(Minus, i)),
                '"' => return self.str(i),
                ',' => return Ok(self.emit1(Comma, i)),
                '<' => return Ok(self.emit1(Less, i)),
                x if x.is_whitespace() => continue,
                x if x.is_alphabetic() => return Ok(self.ident(i)),
                x if x.is_numeric() => return Ok(self.int(i)),
                x => return Err(self.err(BadToken(x))),
            }
        }
    }
}

pub struct Tokens<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Result<Token<'a>>;

    fn next(&mut self) -> Option<Result<Token<'a>>> {
        if self.scanner.at_end() {
            None
        } else {
            Some(self.scanner.scan())
        }
    }
}

pub fn scan<'a>(text: &'a str) -> Tokens<'a> {
    let scanner = Scanner::new(text);
    Tokens { scanner }
}
