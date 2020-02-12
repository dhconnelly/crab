use crate::ast::*;
use crate::code::{Instr, Instr::*};
use std::error;
use std::fmt;
use std::result;

#[derive(Debug)]
pub struct CompileError;

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "compiler:")
    }
}

impl error::Error for CompileError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

type Result<T> = result::Result<T, CompileError>;

struct Compiler {
    instrs: Vec<Instr>,
}

impl Compiler {
    fn new() -> Compiler {
        Compiler { instrs: Vec::new() }
    }

    fn unary_expr(&mut self, op: UnaryOp, expr: &Expr) -> Result<()> {
        use UnaryOp::*;
        self.expr(expr)?;
        match op {
            Minus => self.instrs.push(Negate),
        }
        Ok(())
    }

    fn binary_expr(&mut self, op: BinaryOp, left: &Expr, right: &Expr) -> Result<()> {
        use BinaryOp::*;
        self.expr(left)?;
        self.expr(right)?;
        match op {
            EqEq => self.instrs.push(Eq),
            Star => self.instrs.push(Mul),
            Slash => self.instrs.push(Div),
            Plus => self.instrs.push(Add),
            Minus => self.instrs.push(Sub),
        }
        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> Result<()> {
        use Expr::*;
        match expr {
            Ident(_val) => panic!("not implemented"),
            Int(val) => self.instrs.push(PushInt(*val)),
            Bool(val) => self.instrs.push(PushBool(*val)),
            UnaryExpr(op, expr) => self.unary_expr(*op, expr)?,
            BinaryExpr { op, left, right } => self.binary_expr(*op, left, right)?,
        }
        Ok(())
    }

    fn stmts(&mut self, stmts: &Vec<Stmt>) -> Result<()> {
        for stmt in stmts {
            self.stmt(stmt)?;
        }
        Ok(())
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result<()> {
        use Stmt::*;
        match stmt {
            PrintStmt(expr) => {
                self.expr(expr)?;
                self.instrs.push(Print);
            }
            IfStmt { cond, cons, alt } => {
                self.expr()?;
                let jump_index = self.instrs.len();
                self.instrs.push(JumpIfNot(0));
                self.stmts(cons)?;
                let jump_target = self.instrs.len();
                if alt.is_some() {
                    self.instrs.push(Jump(0));
                    self.stmts(alt.unwrap())?;
                }
            }
        }
        Ok(())
    }

    fn compile(mut self, prog: &Program) -> Result<Vec<Instr>> {
        for stmt in &prog.stmts {
            self.stmt(&stmt)?;
        }
        Ok(self.instrs)
    }
}

pub fn compile(prog: &Program) -> Result<Vec<Instr>> {
    Compiler::new().compile(prog)
}
