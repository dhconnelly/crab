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
        text: String,
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

    fn token_mismatch(want: TokenType, got: Token) -> ParseError {
        TokenMismatch {
            want,
            got: got.typ,
            line: got.line,
            text: got.text.to_string(),
        }
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

    fn peek_is(&mut self, typ: TokenType) -> bool {
        let peek = self.peek();
        peek.is_ok() && peek.unwrap().typ == typ
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

    fn next(&mut self) -> Result<Token<'a>> {
        self.toks.next().unwrap().map_err(|e| e.into())
    }

    fn eat(&mut self, want: TokenType) -> Result<Token> {
        match self.toks.next() {
            Some(Ok(got)) if got.typ == want => Ok(got),
            Some(Ok(got)) => Err(ParseError::token_mismatch(want, got)),
            Some(Err(err)) => Err(err.into()),
            None => Err(UnexpectedEOF),
        }
    }

    fn ident(&mut self, tok: Token<'_>) -> Result<Expr> {
        Ok(Expr::Ident(tok.text.to_string()))
    }

    fn int(&mut self, tok: Token<'_>) -> Result<Expr> {
        let val = tok.text.parse::<i32>()?;
        Ok(Expr::Int(val))
    }

    fn bool(&mut self, val: bool) -> Result<Expr> {
        Ok(Expr::Bool(val))
    }

    fn str(&mut self, tok: Token<'_>) -> Result<Expr> {
        let val = tok.text.to_string();
        Ok(Expr::Str(val))
    }

    fn call(&mut self, tok: Token<'_>) -> Result<Expr> {
        let callee = tok.text.to_string();
        self.eat(LeftParen)?;
        let mut args = Vec::new();
        while !self.peek_is(RightParen) {
            let tok = self.next()?;
            let expr = self.expr(tok)?;
            args.push(expr);
            if !self.peek_is(RightParen) {
                self.eat(Comma)?;
            }
        }
        self.eat(RightParen)?;
        Ok(Expr::CallExpr(callee, args))
    }

    fn terminal(&mut self, tok: Token<'_>) -> Result<Expr> {
        match tok.typ {
            Ident if self.peek_is(LeftParen) => self.call(tok),
            Ident => self.ident(tok),
            Int => self.int(tok),
            Str => self.str(tok),
            True => self.bool(true),
            False => self.bool(false),
            _ => Err(ParseError::bad_expr(tok)),
        }
    }

    fn grouping(&mut self, tok: Token<'_>) -> Result<Expr> {
        if tok.typ == LeftParen {
            let tok = self.next()?;
            let expr = self.expr(tok)?;
            self.eat(RightParen)?;
            Ok(expr)
        } else {
            self.terminal(tok)
        }
    }

    fn unary_expr(&mut self, tok: Token<'_>) -> Result<Expr> {
        if tok.typ == Minus {
            let tok = self.next()?;
            let expr = self.unary_expr(tok)?;
            Ok(Expr::UnaryExpr(UnaryOp::Minus, Box::new(expr)))
        } else {
            self.grouping(tok)
        }
    }

    fn mult_expr(&mut self, tok: Token<'_>) -> Result<Expr> {
        let mut left = self.unary_expr(tok)?;
        while !self.at_end() {
            if let Star | Slash = self.peek()?.typ {
                let op = self.next().unwrap().typ;
                let tok = self.next()?;
                let right = self.unary_expr(tok)?;
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

    fn add_expr(&mut self, tok: Token<'_>) -> Result<Expr> {
        let mut left = self.mult_expr(tok)?;
        while !self.at_end() {
            if let Plus | Minus = self.peek()?.typ {
                let op = self.next().unwrap().typ;
                let tok = self.next()?;
                let right = self.mult_expr(tok)?;
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

    fn binary_op(&mut self, op: BinaryOp, left: Expr) -> Result<Expr> {
        self.next().unwrap();
        let tok = self.next()?;
        let right = self.add_expr(tok)?;
        Ok(Expr::binary(op, Box::new(left), Box::new(right)))
    }

    fn comp_expr(&mut self, tok: Token<'_>) -> Result<Expr> {
        let mut left = self.add_expr(tok)?;
        while !self.at_end() {
            let tok = self.peek()?;
            left = match tok.typ {
                EqEq => self.binary_op(BinaryOp::EqEq, left)?,
                Less => self.binary_op(BinaryOp::Less, left)?,
                _ => break,
            };
        }
        Ok(left)
    }

    fn expr(&mut self, tok: Token<'_>) -> Result<Expr> {
        let expr = self.comp_expr(tok)?;
        Ok(expr)
    }

    fn print_stmt(&mut self) -> Result<Stmt> {
        self.eat(LeftParen)?;
        let tok = self.next()?;
        let expr = self.expr(tok)?;
        self.eat(RightParen)?;
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
        self.eat(LeftParen)?;
        let tok = self.next()?;
        let cond = self.expr(tok)?;
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

    fn let_stmt(&mut self) -> Result<Stmt> {
        let ident = self.eat(Ident)?.text.to_string();
        self.eat(Eq)?;
        let tok = self.next()?;
        let expr = self.expr(tok)?;
        self.eat(Semicolon)?;
        Ok(Stmt::LetStmt(ident, expr))
    }

    fn assign_stmt(&mut self, ident: &str) -> Result<Stmt> {
        self.eat(Eq)?;
        let tok = self.next()?;
        let expr = self.expr(tok)?;
        self.eat(Semicolon)?;
        Ok(Stmt::AssignStmt(ident.to_string(), expr))
    }

    fn expr_stmt(&mut self, tok: Token<'_>) -> Result<Stmt> {
        let expr = self.expr(tok)?;
        self.eat(Semicolon)?;
        Ok(Stmt::ExprStmt(expr))
    }

    fn return_stmt(&mut self) -> Result<Stmt> {
        let tok = self.next()?;
        let expr = self.expr(tok)?;
        self.eat(Semicolon)?;
        Ok(Stmt::ReturnStmt(expr))
    }

    fn stmt(&mut self) -> Result<Stmt> {
        let tok = self.next()?;
        match tok.typ {
            Print => self.print_stmt(),
            If => self.if_stmt(),
            Let => self.let_stmt(),
            Ident if self.peek_is(Eq) => self.assign_stmt(tok.text),
            Return => self.return_stmt(),
            _ => self.expr_stmt(tok),
        }
    }

    fn fn_def(&mut self) -> Result<Stmt> {
        self.eat(Func)?;
        let name = self.eat(Ident)?.text.to_string();
        self.eat(LeftParen)?;
        let mut params = Vec::new();
        while !self.peek_is(RightParen) {
            params.push(self.eat(Ident)?.text.to_string());
            if !self.peek_is(RightParen) {
                self.eat(Comma)?;
            }
        }
        self.eat(RightParen)?;
        let body = self.block()?;
        Ok(Stmt::FnDefStmt(name, params, body))
    }

    fn def(&mut self) -> Result<Stmt> {
        match self.peek()?.typ {
            Func => self.fn_def(),
            _ => self.stmt(),
        }
    }

    fn program(&mut self) -> Result<Program> {
        let mut stmts = Vec::new();
        while !self.at_end() {
            stmts.push(self.def()?);
        }
        Ok(Program { stmts })
    }
}

pub fn parse<'a>(toks: Tokens<'a>) -> Result<Program> {
    Parser::new(toks).program()
}
