use crate::code::Instr;
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::result;

#[derive(Debug)]
pub enum ExecutionError {
    StackEmptyErr,
    TypeMismatch { want: Type, got: TypedValue },
    InvalidGlobal(usize),
    StackOutOfBounds(usize),
    ArgumentCountMismatch(String, usize, usize),
    BadFunction(String),
}
use ExecutionError::*;

impl ExecutionError {
    fn type_mismatch(want: Type, got: TypedValue) -> ExecutionError {
        TypeMismatch { want, got }
    }
}

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

// TODO: make all of this type stuff nicer using traits and so on

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Bool,
    Str,
    Addr,
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Bool(bool),
    Str(String),
    Addr(usize),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(x) => write!(f, "{}", x),
            Value::Bool(x) => write!(f, "{}", x),
            Value::Str(x) => write!(f, "{}", x),
            Value::Addr(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedValue {
    pub typ: Type,
    pub val: Value,
}

impl TypedValue {
    fn int(x: i32) -> TypedValue {
        TypedValue {
            typ: Type::Int,
            val: Value::Int(x),
        }
    }

    fn addr(x: usize) -> TypedValue {
        TypedValue {
            typ: Type::Addr,
            val: Value::Addr(x),
        }
    }

    fn bool(x: bool) -> TypedValue {
        TypedValue {
            typ: Type::Bool,
            val: Value::Bool(x),
        }
    }

    fn str(x: String) -> TypedValue {
        TypedValue {
            typ: Type::Str,
            val: Value::Str(x),
        }
    }
}

impl TypedValue {
    fn as_int(self) -> Result<i32> {
        if let Value::Int(x) = self.val {
            Ok(x)
        } else {
            Err(ExecutionError::type_mismatch(Type::Int, self))
        }
    }

    fn as_addr(self) -> Result<usize> {
        if let Value::Addr(x) = self.val {
            Ok(x)
        } else {
            Err(ExecutionError::type_mismatch(Type::Addr, self))
        }
    }

    fn as_bool(self) -> Result<bool> {
        if let Value::Bool(x) = self.val {
            Ok(x)
        } else {
            Err(ExecutionError::type_mismatch(Type::Bool, self))
        }
    }

    fn as_str(self) -> Result<String> {
        if let Value::Str(x) = self.val {
            Ok(x)
        } else {
            Err(ExecutionError::type_mismatch(Type::Str, self))
        }
    }
}

struct Function {
    name: String,
    addr: usize,
    params: usize,
}

struct VM<'a> {
    stack: Vec<TypedValue>,
    sps: Vec<usize>,
    globals: HashMap<usize, TypedValue>,
    functions: HashMap<String, Function>,
    returns: Vec<usize>,
    code: &'a [Instr],
    pc: usize,
}

impl VM<'_> {
    fn new(code: &[Instr]) -> VM {
        VM {
            stack: Vec::new(),
            sps: Vec::new(),
            globals: HashMap::new(),
            code,
            pc: 0,
            returns: Vec::new(),
            functions: HashMap::new(),
        }
    }

    fn at_end(&self) -> bool {
        self.pc >= self.code.len()
    }

    fn pop_val(&mut self) -> Result<TypedValue> {
        let top = self.stack.pop();
        match top {
            None => Err(StackEmptyErr),
            Some(val) => Ok(val),
        }
    }

    fn pop_int(&mut self) -> Result<i32> {
        self.pop_val()?.as_int()
    }

    fn pop_addr(&mut self) -> Result<usize> {
        self.pop_val()?.as_addr()
    }

    fn pop_bool(&mut self) -> Result<bool> {
        self.pop_val()?.as_bool()
    }

    fn pop_str(&mut self) -> Result<String> {
        self.pop_val()?.as_str()
    }

    fn step(&mut self) -> Result<()> {
        use Instr::*;
        let instr = &self.code[self.pc];
        match instr {
            Call(name, args) => match self.functions.get(name) {
                None => return Err(BadFunction(name.to_string())),
                Some(func) if func.params != *args => {
                    return Err(ArgumentCountMismatch(name.to_string(), func.params, *args));
                }
                Some(func) => {
                    self.returns.push(self.pc);
                    self.pc = func.addr;
                    self.sps.push(self.stack.len());
                }
            },

            Def(name, params) => {
                self.functions.insert(
                    name.to_string(),
                    Function {
                        name: name.to_string(),
                        params: *params,
                        addr: self.pc,
                    },
                );
            }

            Return => {
                let addr = self.returns.pop().expect("no return address!");
                self.pc = addr;
                let retval = self.pop_val()?;
                if let Some(sp) = self.sps.pop() {
                    self.stack.truncate(sp);
                }
                self.stack.push(retval);
            }

            GetStack(i) => {
                let i = self.sps.last().unwrap_or(&0) + i;
                let val = self.stack.get(i).ok_or(StackOutOfBounds(i))?.clone();
                self.stack.push(val);
                self.pc += 1;
            }

            SetStack(i) => {
                let i = self.sps.last().unwrap_or(&0) + i;
                let val = self.pop_val()?;
                let cur = self.stack.get_mut(i).ok_or(StackOutOfBounds(i))?;
                *cur = val;
                self.pc += 1;
            }

            PopStack(i) => {
                for _ in 0..*i {
                    self.stack.pop();
                }
                self.pc += 1;
            }

            PushInt(val) => {
                self.stack.push(TypedValue::int(*val));
                self.pc += 1;
            }

            PushBool(val) => {
                self.stack.push(TypedValue::bool(*val));
                self.pc += 1;
            }

            PushStr(val) => {
                self.stack.push(TypedValue::str(val.clone()));
                self.pc += 1;
            }

            GetGlobal(i) => {
                let val = self.globals.get(i).ok_or(InvalidGlobal(*i))?;
                self.stack.push(val.clone());
                self.pc += 1;
            }

            SetGlobal(i) => {
                let val = self.pop_val()?;
                self.globals.insert(*i, val);
                self.pc += 1;
            }

            Negate => {
                let val = TypedValue::int(-self.pop_int()?);
                self.stack.push(val);
                self.pc += 1;
            }

            Add => {
                let right = self.pop_int()?;
                let left = self.pop_int()?;
                self.stack.push(TypedValue::int(left + right));
                self.pc += 1;
            }

            Sub => {
                let right = self.pop_int()?;
                let left = self.pop_int()?;
                self.stack.push(TypedValue::int(left - right));
                self.pc += 1;
            }

            Mul => {
                let right = self.pop_int()?;
                let left = self.pop_int()?;
                self.stack.push(TypedValue::int(left * right));
                self.pc += 1;
            }

            Div => {
                let right = self.pop_int()?;
                let left = self.pop_int()?;
                self.stack.push(TypedValue::int(left / right));
                self.pc += 1;
            }

            Eq => {
                let right = self.pop_val()?;
                let eq = match right.val {
                    Value::Int(x) => self.pop_int()? == x,
                    Value::Bool(x) => self.pop_bool()? == x,
                    Value::Str(x) => self.pop_str()? == x,
                    Value::Addr(x) => panic!("address comparison not supported"),
                };
                self.stack.push(TypedValue::bool(eq));
                self.pc += 1;
            }

            Less => {
                panic!("not implemented: Less");
            }

            Print => {
                let val = self.pop_val()?;
                println!("{}", val.val);
                self.pc += 1;
            }

            Jump => {
                self.pc = self.pop_addr()?;
            }

            JumpIfNot => {
                let addr = self.pop_addr()?;
                if !self.pop_bool()? {
                    self.pc = addr;
                } else {
                    self.pc += 1;
                }
            }

            PushAddr(x) => {
                self.stack.push(TypedValue::addr(*x));
                self.pc += 1;
            }

            PushCur => {
                self.stack.push(TypedValue::addr(self.pc));
                self.pc += 1;
            }
        }
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        while !self.at_end() {
            self.step()?;
        }
        Ok(())
    }
}

pub fn execute(code: &[Instr]) -> Result<()> {
    VM::new(code).run()
}
