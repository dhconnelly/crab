#[derive(Debug)]
pub enum Instr {
    PushInt(i32),
    PushBool(bool),
    PushStr(String),
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
