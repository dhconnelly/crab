use crate::ast::*;
use crate::code::{Instr, Instr::*};
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::result;

#[derive(Debug)]
pub enum CompileError {
    Undefined(String),
    Redefinition(String),
}
use CompileError::*;

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = match self {
            Undefined(ident) => format!("undefined: {}", ident),
            Redefinition(ident) => format!("redefining: {}", ident),
        };
        write!(f, "compiler: {}", msg)
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
    globals: HashMap<String, usize>,
    stack_frames: Vec<HashMap<String, usize>>,
    stack_depth: usize,
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            instrs: Vec::new(),
            globals: HashMap::new(),
            stack_frames: Vec::new(),
            stack_depth: 0,
        }
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

    fn is_defined(&self, ident: &str) -> bool {
        match self.stack_frames.last() {
            None => self.globals.contains_key(ident),
            Some(frame) => frame.contains_key(ident),
        }
    }

    fn define(&mut self, ident: &str) {
        match self.stack_frames.last_mut() {
            None => {
                let i = self.globals.len();
                self.globals.insert(ident.to_string(), i);
                self.instrs.push(SetGlobal(i));
            }
            Some(frame) => {
                frame.insert(ident.to_string(), self.stack_depth);
                self.stack_depth += 1;
            }
        }
    }

    fn set(&mut self, ident: &str) -> Result<()> {
        match self.find_frame(ident) {
            None => {
                let i = self
                    .globals
                    .get(ident)
                    .ok_or(Undefined(ident.to_string()))?;
                self.instrs.push(SetGlobal(*i));
            }
            Some(frame) => {
                let i = *frame.get(ident).unwrap();
                self.instrs.push(SetStack(i));
            }
        }
        Ok(())
    }

    fn find_frame(&self, ident: &str) -> Option<&HashMap<String, usize>> {
        self.stack_frames
            .iter()
            .rev()
            .find(|m| m.contains_key(ident))
    }

    fn get(&mut self, ident: &str) -> Result<()> {
        match self.find_frame(ident) {
            Some(frame) => {
                let i = *frame.get(ident).unwrap();
                Ok(self.instrs.push(GetStack(i)))
            }
            None => {
                let i = self
                    .globals
                    .get(ident)
                    .ok_or(Undefined(ident.to_string()))?;
                Ok(self.instrs.push(GetGlobal(*i)))
            }
        }
    }

    fn expr(&mut self, expr: &Expr) -> Result<()> {
        use Expr::*;
        match expr {
            Ident(val) => self.get(val)?,
            Int(val) => self.instrs.push(PushInt(*val)),
            Bool(val) => self.instrs.push(PushBool(*val)),
            Str(val) => self.instrs.push(PushStr(val.clone())),
            UnaryExpr(op, expr) => self.unary_expr(*op, expr)?,
            BinaryExpr { op, left, right } => self.binary_expr(*op, left, right)?,
        }
        Ok(())
    }

    fn block(&mut self, Block(stmts): &Block) -> Result<()> {
        for stmt in stmts {
            self.stmt(stmt)?;
        }
        Ok(())
    }

    fn push_frame(&mut self) {
        self.stack_frames.push(HashMap::new());
    }

    fn pop_frame(&mut self) {
        let frame = self.stack_frames.pop();
        self.instrs.push(PopStack(frame.as_ref().unwrap().len()));
        self.stack_depth = self.stack_depth - frame.unwrap().len();
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result<()> {
        use Stmt::*;
        match stmt {
            PrintStmt(expr) => {
                self.expr(expr)?;
                self.instrs.push(Print);
            }

            IfStmt { cond, cons, alt } => {
                self.push_frame();
                self.expr(cond)?;
                let skip_cons_pc = self.instrs.len();
                self.instrs.push(JumpIfNot(0));
                self.block(cons)?;
                if alt.is_some() {
                    let skip_alt_pc = self.instrs.len();
                    self.instrs.push(Jump(0));
                    let after_cons_pc = self.instrs.len();
                    self.instrs[skip_cons_pc] = JumpIfNot(after_cons_pc);
                    self.block(alt.as_ref().unwrap())?;
                    let after_alt_pc = self.instrs.len();
                    self.instrs[skip_alt_pc] = Jump(after_alt_pc);
                } else {
                    let after_cons_pc = self.instrs.len();
                    self.instrs[skip_cons_pc] = JumpIfNot(after_cons_pc);
                }
                self.pop_frame();
            }

            LetStmt(ident, expr) => {
                if self.is_defined(ident) {
                    return Err(Redefinition(ident.to_string()));
                }
                self.expr(expr)?;
                self.define(ident);
            }

            AssignStmt(ident, expr) => {
                self.expr(expr)?;
                self.set(ident)?;
            }

            ExprStmt(expr) => {
                self.expr(expr)?;
                self.instrs.push(PopStack(1));
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
