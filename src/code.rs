#[derive(Debug)]
pub enum Instr {
    PushInt(i32),
    PushBool(bool),
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
