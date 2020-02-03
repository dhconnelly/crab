use crate::ast::Program;
use crate::token::Token;
use std::error;
use std::fmt;
use std::iter::Peekable;
use std::result;

#[derive(Debug)]
pub struct ParseError {
    pub line: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parser: line {}:", self.line)
    }
}

impl error::Error for ParseError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

type Result<T> = result::Result<T, ParseError>;

pub fn parse<'a>(toks: impl Iterator<Item = Token<'a>>) -> Result<Program> {
    Err(ParseError { line: 1 })
}
