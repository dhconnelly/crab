use crate::ast::*;
use crate::scanner;
use crate::scanner::Tokens;
use crate::token::{Token, TokenType, TokenType::*};
use std::error;
use std::fmt;
use std::iter::Peekable;
use std::num;
use std::result;

// TODO: carry line and column information
#[derive(Debug)]
pub enum ParseError {
    UnexpectedEOF,
    TokenMismatch {
        want: TokenType,
        got: TokenType,
        line: usize,
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

    fn token_mismatch(want: TokenType, got: TokenType, line: usize) -> ParseError {
        TokenMismatch { want, got, line }
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

    fn peek(&mut self) -> Result<&Token> {
        match self.toks.peek() {
            Some(Ok(tok)) => Ok(tok),
            Some(Err(err)) => Err(ScanError(*err)),
            None => Err(UnexpectedEOF),
        }
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

    fn eat(&mut self, want: TokenType) -> Result<Token> {
        match self.toks.next() {
            Some(Ok(got)) if got.typ == want => Ok(got),
            Some(Ok(got)) => Err(ParseError::token_mismatch(want, got.typ, got.line)),
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

    fn bool(&mut self, val: bool) -> Result<Expr> {
        self.next().unwrap();
        Ok(Expr::Bool(val))
    }

    fn str(&mut self) -> Result<Expr> {
        let tok = self.eat(Str)?;
        let val = tok.text.to_string();
        Ok(Expr::Str(val))
    }

    fn terminal(&mut self) -> Result<Expr> {
        match self.peek()?.typ {
            Ident => self.ident(),
            Int => self.int(),
            Str => self.str(),
            True => self.bool(true),
            False => self.bool(false),
            _ => Err(ParseError::bad_expr(self.next().unwrap())),
        }
    }

    fn grouping(&mut self) -> Result<Expr> {
        if self.peek()?.typ == LeftParen {
            self.eat(LeftParen).unwrap();
            let expr = self.expr()?;
            self.eat(RightParen)?;
            Ok(expr)
        } else {
            self.terminal()
        }
    }

    fn unary_expr(&mut self) -> Result<Expr> {
        if self.peek()?.typ == Minus {
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
            if let Star | Slash = self.peek()?.typ {
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
            if let Plus | Minus = self.peek()?.typ {
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
            if self.peek()?.typ == EqEq {
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

    fn block(&mut self) -> Result<Block> {
        self.eat(LeftBrace)?;
        let mut stmts = Vec::new();
        if !self.at_end() && self.peek()?.typ != RightBrace {
            while self.peek()?.typ != RightBrace {
                stmts.push(self.stmt()?);
            }
        }
        self.eat(RightBrace)?;
        Ok(Block(stmts))
    }

    fn if_stmt(&mut self) -> Result<Stmt> {
        self.eat(If).unwrap();
        self.eat(LeftParen)?;
        let cond = self.expr()?;
        self.eat(RightParen)?;
        let cons = self.block()?;
        let alt = if let Ok(Token { typ: Else, .. }) = self.peek() {
            self.eat(Else).unwrap();
            Some(self.block()?)
        } else {
            None
        };
        Ok(Stmt::IfStmt { cond, cons, alt })
    }

    fn stmt(&mut self) -> Result<Stmt> {
        let tok = self.peek()?;
        match tok.typ {
            Print => self.print_stmt(),
            If => self.if_stmt(),
            // TODO: use a different error
            _ => Err(ParseError::bad_expr(self.next().unwrap())),
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
