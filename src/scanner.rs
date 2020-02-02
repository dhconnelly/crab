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

    fn emit(&self, typ: TokenType, from: usize, until: usize) -> Token<'a> {
        Token {
            typ,
            text: &self.text[from..until],
            line: self.line,
        }
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

    fn ident(&mut self, from: usize) -> Result<Token<'a>, ScanError> {
        panic!();
    }

    fn scan(&mut self) -> Result<Token<'a>, ScanError> {
        loop {
            if self.at_end() {
                let tok = self.emit(EOF, self.text.len(), self.text.len());
                return Ok(tok);
            }
            let tok = match self.chars.next().unwrap() {
                (_, '\n') => {
                    self.line += 1;
                    continue;
                }
                (_, x) if x.is_whitespace() => continue,
                (i, x) if x.is_alphabetic() => self.ident(i)?,
                (i, '(') => self.emit(LeftParen, i, i + 1),
                (i, ')') => self.emit(RightParen, i, i + 1),
                (i, ';') => self.emit(Semicolon, i, i + 1),
                (i, '=') => {
                    self.eat(EqEq, '=')?;
                    self.emit(EqEq, i, i + 2)
                }
                (i, '*') => self.emit(Star, i, i + 1),
                (i, '/') => self.emit(Slash, i, i + 1),
                (i, '+') => self.emit(Plus, i, i + 1),
                (i, '-') => self.emit(Minus, i, i + 1),
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
