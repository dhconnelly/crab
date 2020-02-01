#[derive(Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
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

#[derive(Debug)]
pub struct BinaryExpr {
    op: BinaryOp,
    left: Box<Expr>,
    right: Box<Expr>,
}

#[derive(Debug)]
pub struct UnaryExpr {
    op: UnaryOp,
    right: Box<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Ident(String),
    UnaryExpr(UnaryExpr),
    BinaryExpr(BinaryExpr),
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
}
