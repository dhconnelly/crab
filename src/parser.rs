use crate::ast::{Stmt::*, *};
use crate::scanner;
use crate::scanner::Tokens;
use crate::token::{Token, TokenType, TokenType::*};
use std::error;
use std::fmt;
use std::iter::Peekable;
use std::result;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEOF,
    TokenMismatch { want: TokenType, got: TokenType },
    ScanError(scanner::ScanError),
}
use ParseError::*;

impl ParseError {
    fn token_mismatch(want: TokenType, got: TokenType) -> ParseError {
        TokenMismatch { want, got }
    }
}

impl From<scanner::ScanError> for ParseError {
    fn from(err: scanner::ScanError) -> ParseError {
        ScanError(err)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parser:")
    }
}

impl error::Error for ParseError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

type Result<T> = result::Result<T, ParseError>;

struct Parser<'a> {
    toks: Peekable<Tokens<'a>>,
}

impl<'a> Parser<'a> {
    fn new(toks: Tokens<'a>) -> Parser<'a> {
        let toks = toks.peekable();
        Parser { toks }
    }

    fn expr(&mut self) -> Result<Expr> {
        panic!();
    }

    fn eat(&mut self, want: TokenType) -> Result<()> {
        match self.peek()? {
            Some(got) if want == got => Ok(()),
            Some(got) => Err(ParseError::token_mismatch(want, got)),
            None => Err(UnexpectedEOF),
        }
    }

    fn peek(&mut self) -> Result<Option<TokenType>> {
        match self.toks.peek() {
            Some(Err(err)) => Err(ScanError(*err)),
            Some(Ok(tok)) => Ok(Some(tok.typ)),
            None => Ok(None),
        }
    }

    fn print_stmt(&mut self) -> Result<Stmt> {
        self.eat(Print)?;
        let expr = self.expr()?;
        self.eat(Semicolon)?;
        Ok(PrintStmt(expr))
    }

    fn stmt(&mut self) -> Result<Stmt> {
        match self.peek()? {
            None => Err(UnexpectedEOF),
            Some(Print) => self.print_stmt(),
            Some(tok) => Err(ParseError::token_mismatch(Print, tok)),
        }
    }

    fn program(&mut self) -> Result<Program> {
        let mut stmts = Vec::new();
        while self.toks.peek().is_some() {
            stmts.push(self.stmt()?);
        }
        Ok(Program { stmts })
    }
}

pub fn parse<'a>(toks: Tokens<'a>) -> Result<Program> {
    Parser::new(toks).program()
}
