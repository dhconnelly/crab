#[derive(Debug)]
pub enum Instr {
    PushInt(i32),
    Negate,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Print,
}
