use crate::code::Instr;
use std::convert;
use std::convert::TryInto;
use std::error;
use std::fmt;
use std::result;

#[derive(Debug)]
pub enum ExecutionError {
    StackEmptyErr,
    TypeMismatch { want: Type, got: Type },
}
use ExecutionError::*;

impl fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vm:")
    }
}

impl error::Error for ExecutionError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

type Result<T> = result::Result<T, ExecutionError>;

#[derive(Debug)]
enum Type {
    Int,
    Bool,
}

#[derive(Debug)]
enum Value {
    Int(i32),
    Bool(bool),
}

#[derive(Debug)]
struct TypedValue {
    typ: Type,
    val: Value,
}

impl TypedValue {
    fn int(x: i32) -> TypedValue {
        TypedValue {
            typ: Type::Int,
            val: Value::Int(x),
        }
    }
}

impl convert::TryFrom<TypedValue> for i32 {
    type Error = ExecutionError;

    fn try_from(val: TypedValue) -> Result<i32> {
        if let Value::Int(x) = val.val {
            Ok(x)
        } else {
            Err(ExecutionError::TypeMismatch {
                want: Type::Int,
                got: val.typ,
            })
        }
    }
}

impl convert::TryFrom<TypedValue> for bool {
    type Error = ExecutionError;

    fn try_from(val: TypedValue) -> Result<bool> {
        if let Value::Bool(x) = val.val {
            Ok(x)
        } else {
            Err(ExecutionError::TypeMismatch {
                want: Type::Bool,
                got: val.typ,
            })
        }
    }
}

struct VM<'a> {
    stack: Vec<TypedValue>,
    code: &'a [Instr],
    pc: usize,
}

impl VM<'_> {
    fn new(code: &[Instr]) -> VM {
        VM {
            stack: Vec::new(),
            code,
            pc: 0,
        }
    }

    fn pop_val<T>(&mut self) -> Result<T> {
        let top = self.stack.pop();
        match top {
            None => Err(StackEmptyErr),
            Some(val) => Ok(val.try_into()?),
        }
    }

    fn step(&mut self) -> Result<()> {
        use Instr::*;
        let instr = self.code[self.pc];
        match instr {
            PushInt(val) => self.stack.push(TypedValue::int(val)),
            Negate => self.stack.push(TypedValue::int(-self.pop_val()?)),
            Add => panic!("not implemented"),
            Sub => panic!("not implemented"),
            Mul => panic!("not implemented"),
            Div => panic!("not implemented"),
            Eq => panic!("not implemented"),
            Print => panic!("not implemented"),
        }
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        self.step()
    }
}

pub fn execute(code: &[Instr]) -> Result<()> {
    VM::new(code).run()
}
