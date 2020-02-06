#[derive(Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    PrintStmt(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Ident(String),
    Int(i32),
    UnaryExpr(UnaryOp, Box<Expr>),
    BinaryExpr {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

impl Expr {
    pub fn binary(op: BinaryOp, left: Box<Expr>, right: Box<Expr>) -> Self {
        Self::BinaryExpr { op, left, right }
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    EqEq,
    Star,
    Slash,
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum UnaryOp {
    Minus,
}
