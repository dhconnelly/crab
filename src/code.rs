#[derive(Debug)]
pub enum Instr {
    PushInt(i32),
    PushBool(bool),
    PushStr(String),
    Get(usize),
    Def(usize),
    Negate,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Print,
    Jump(usize),
    JumpIfNot(usize),
}
