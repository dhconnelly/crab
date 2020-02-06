use crate::ast::*;
use crate::scanner;
use crate::scanner::Tokens;
use crate::token::{Token, TokenType, TokenType::*};
use std::error;
use std::fmt;
use std::iter::Peekable;
use std::num;
use std::result;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEOF,
    TokenMismatch {
        want: TokenType,
        got: TokenType,
    },
    ScanError(scanner::ScanError),
    BadExpr {
        line: usize,
        text: String,
        typ: TokenType,
    },
    ParseIntError(num::ParseIntError),
}
use ParseError::*;

impl ParseError {
    fn bad_expr(tok: Token<'_>) -> ParseError {
        BadExpr {
            line: tok.line,
            text: tok.text.to_string(),
            typ: tok.typ,
        }
    }

    fn token_mismatch(want: TokenType, got: TokenType) -> ParseError {
        TokenMismatch { want, got }
    }
}

impl From<scanner::ScanError> for ParseError {
    fn from(err: scanner::ScanError) -> ParseError {
        ScanError(err)
    }
}

impl From<num::ParseIntError> for ParseError {
    fn from(err: num::ParseIntError) -> ParseError {
        ParseIntError(err)
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

    fn at_end(&mut self) -> bool {
        match self.toks.peek() {
            None => true,
            Some(Ok(tok)) if tok.typ == EOF => true,
            _ => false,
        }
    }

    fn next(&mut self) -> Result<Token> {
        self.toks.next().unwrap().map_err(|e| e.into())
    }

    fn peek(&mut self) -> Result<TokenType> {
        match self.toks.peek() {
            Some(Ok(tok)) => Ok(tok.typ),
            Some(Err(err)) => Err(ScanError(*err)),
            None => Err(UnexpectedEOF),
        }
    }

    fn eat(&mut self, want: TokenType) -> Result<Token> {
        match self.toks.next() {
            Some(Ok(got)) if got.typ == want => Ok(got),
            Some(Ok(got)) => Err(ParseError::token_mismatch(want, got.typ)),
            Some(Err(err)) => Err(err.into()),
            None => Err(UnexpectedEOF),
        }
    }

    fn ident(&mut self) -> Result<Expr> {
        let tok = self.eat(Ident)?;
        Ok(Expr::Ident(tok.text.to_string()))
    }

    fn int(&mut self) -> Result<Expr> {
        let tok = self.eat(Int)?;
        let val = tok.text.parse::<i32>()?;
        Ok(Expr::Int(val))
    }

    fn terminal(&mut self) -> Result<Expr> {
        match self.peek()? {
            Ident => self.ident(),
            Int => self.int(),
            _ => Err(ParseError::bad_expr(self.next().unwrap())),
        }
    }

    fn grouping(&mut self) -> Result<Expr> {
        if self.peek()? == LeftParen {
            self.eat(LeftParen).unwrap();
            let expr = self.expr()?;
            self.eat(RightParen)?;
            Ok(expr)
        } else {
            self.terminal()
        }
    }

    fn unary_expr(&mut self) -> Result<Expr> {
        if self.peek()? == Minus {
            self.eat(Minus).unwrap();
            let expr = self.unary_expr()?;
            Ok(Expr::UnaryExpr(UnaryOp::Minus, Box::new(expr)))
        } else {
            self.grouping()
        }
    }

    fn mult_expr(&mut self) -> Result<Expr> {
        let mut left = self.unary_expr()?;
        while !self.at_end() {
            if let Star | Slash = self.peek()? {
                let op = self.next().unwrap().typ;
                let right = self.unary_expr()?;
                let op = match op {
                    Star => BinaryOp::Star,
                    Slash => BinaryOp::Slash,
                    _ => panic!(),
                };
                left = Expr::binary(op, Box::new(left), Box::new(right));
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn add_expr(&mut self) -> Result<Expr> {
        let mut left = self.mult_expr()?;
        while !self.at_end() {
            if let Plus | Minus = self.peek()? {
                let op = self.next().unwrap().typ;
                let right = self.mult_expr()?;
                let op = match op {
                    Plus => BinaryOp::Plus,
                    Minus => BinaryOp::Minus,
                    _ => panic!(),
                };
                left = Expr::binary(op, Box::new(left), Box::new(right));
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn comp_expr(&mut self) -> Result<Expr> {
        let mut left = self.add_expr()?;
        while !self.at_end() {
            if self.peek()? == EqEq {
                self.eat(EqEq).unwrap();
                let right = self.add_expr()?;
                left = Expr::binary(BinaryOp::EqEq, Box::new(left), Box::new(right));
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn expr(&mut self) -> Result<Expr> {
        self.comp_expr()
    }

    fn print_stmt(&mut self) -> Result<Stmt> {
        self.eat(Print)?;
        let expr = self.expr()?;
        self.eat(Semicolon)?;
        Ok(Stmt::PrintStmt(expr))
    }

    fn stmt(&mut self) -> Result<Stmt> {
        match self.peek()? {
            Print => self.print_stmt(),
            tok => Err(ParseError::token_mismatch(Print, tok)),
        }
    }

    fn program(&mut self) -> Result<Program> {
        let mut stmts = Vec::new();
        while !self.at_end() {
            stmts.push(self.stmt()?);
        }
        Ok(Program { stmts })
    }
}

pub fn parse<'a>(toks: Tokens<'a>) -> Result<Program> {
    Parser::new(toks).program()
}
