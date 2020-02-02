use crate::token::{Token, TokenType, TokenType::*};
use std::error;
use std::fmt;
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug)]
pub enum ScanErrType {
    BadToken(char),
    TokenMismatch { want: TokenType, got: char },
    EarlyEOF { want: TokenType },
}
use ScanErrType::*;

#[derive(Debug)]
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
            TokenMismatch { want, got } => format!("want {:?}, got {}", want, got),
            EarlyEOF { want } => format!("unexpected EOF, want {:?}", want),
        };
        write!(f, "scanner: line {}: {}", self.line, msg)
    }
}

struct Scanner<'a> {
    text: &'a str,
    chars: Peekable<CharIndices<'a>>,
    line: usize,
}

impl<'a> Scanner<'a> {
    fn new(text: &'a str) -> Scanner<'a> {
        Scanner {
            text,
            chars: text.char_indices().peekable(),
            line: 1,
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, x)| *x)
    }

    fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn err(&self, typ: ScanErrType) -> ScanError {
        ScanError {
            line: self.line,
            typ,
        }
    }

    fn emit(&self, typ: TokenType, text: &'a str) -> Token<'a> {
        Token {
            typ,
            text,
            line: self.line,
        }
    }

    fn text(&self, from: usize, until: usize) -> &'a str {
        &self.text[from..until]
    }

    fn eat(&mut self, want: TokenType, ch: char) -> Result<(), ScanError> {
        match self.peek() {
            None => Err(self.err(EarlyEOF { want })),
            Some(x) if x == ch => {
                self.chars.next();
                Ok(())
            }
            Some(x) => Err(self.err(TokenMismatch { want, got: x })),
        }
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
            ident => Ident,
        };
        self.emit(typ, text)
    }

    fn int(&mut self, from: usize) -> Token<'a> {
        let text = self.eat_while(from, |ch| ch.is_numeric());
        self.emit(Int, text)
    }

    fn scan(&mut self) -> Result<Token<'a>, ScanError> {
        loop {
            if self.at_end() {
                let text = self.text(self.text.len(), self.text.len());
                let tok = self.emit(EOF, text);
                return Ok(tok);
            }
            let tok = match self.chars.next().unwrap() {
                (_, '\n') => {
                    self.line += 1;
                    continue;
                }
                (_, x) if x.is_whitespace() => continue,
                (i, x) if x.is_alphabetic() => self.ident(i),
                (i, x) if x.is_numeric() => self.int(i),
                (i, '(') => self.emit(LeftParen, self.text(i, i + 1)),
                (i, ')') => self.emit(RightParen, self.text(i, i + 1)),
                (i, ';') => self.emit(Semicolon, self.text(i, i + 1)),
                (i, '=') => {
                    self.eat(EqEq, '=')?;
                    self.emit(EqEq, self.text(i, i + 2))
                }
                (i, '*') => self.emit(Star, self.text(i, i + 1)),
                (i, '/') => {
                    if self.peek().is_some() && self.peek().unwrap() == '/' {
                        self.eat_while(i, |ch| ch != '\n');
                        continue;
                    }
                    self.emit(Slash, self.text(i, i + 1))
                }
                (i, '+') => self.emit(Plus, self.text(i, i + 1)),
                (i, '-') => self.emit(Minus, self.text(i, i + 1)),
                (_, x) => return Err(self.err(BadToken(x))),
            };
            return Ok(tok);
        }
    }
}

pub fn scan<'a>(text: &'a str) -> Result<Vec<Token<'a>>, ScanError> {
    let mut scanner = Scanner::new(text);
    let mut toks: Vec<Token<'a>> = Vec::new();
    loop {
        let tok: Token<'a> = scanner.scan()?;
        toks.push(tok);
        if toks.last().unwrap().typ == EOF {
            break;
        }
    }
    Ok(toks)
}
