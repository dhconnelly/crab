#[derive(Debug)]
pub enum Instr {
    GetStack(usize),
    PushInt(i32),
    PushBool(bool),
    PushStr(String),
    GetGlobal(usize),
    DefGlobal(usize),
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
