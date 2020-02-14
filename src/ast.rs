#[derive(Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Block(pub Vec<Stmt>);

#[derive(Debug)]
pub enum Stmt {
    PrintStmt(Expr),
    IfStmt {
        cond: Expr,
        cons: Block,
        alt: Option<Block>,
    },
    LetStmt(String, Expr),
    AssignStmt(String, Expr),
    ExprStmt(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Ident(String),
    Int(i32),
    Bool(bool),
    Str(String),
    UnaryExpr(UnaryOp, Box<Expr>),
    BinaryExpr {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

impl Expr {
    pub fn binary(op: BinaryOp, left: Box<Expr>, right: Box<Expr>) -> Expr {
        Expr::BinaryExpr { op, left, right }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    EqEq,
    Star,
    Slash,
    Plus,
    Minus,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Minus,
}
