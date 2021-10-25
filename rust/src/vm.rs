use std::error::Error;
use std::fmt::Display;

use crate::compiler::{Compiler, Function};
use crate::object::{Memory, MemoryEntry};
use crate::value::Value;
use crate::value::Value::*;

const MAX_FRAMES: usize = 265;

/// Lox Virtual Machine.
///
/// This struct contains the VM runtime properties that
/// track the stack and objects.
///
/// In the C version of the interpreter, the VM is a global
/// variable, and it contains all of the values used by the
/// interpreter at runtime. This struct contains those same
/// values, but is initialized by the caller.
///
/// # Example Use
///
/// ```
/// # use loxvm::vm::VM;
/// let mut vm = VM::default();
/// vm.interpret("print \"Hello, world!\"");
/// ```
#[derive(Debug)]
pub struct VM {
    stack: Vec<Value>,

    // todo: replace all pub with pub(crate) where possible
    pub memory: Memory,

    frames: Vec<CallFrame>,
    ip: usize,
}

#[derive(Debug)]
struct CallFrame {
    closure: MemoryEntry,
    ip: usize,
    slots: usize,
}

#[derive(Debug)]
pub struct LoxErrorSpec {
    pub line: usize,
    pub message: String,
}

#[derive(Debug)]
pub enum LoxError {
    ScanError(LoxErrorSpec),
    ParseError(LoxErrorSpec),
    RuntimeError(LoxErrorSpec),
}

#[derive(Debug)]
pub struct LoxErrorChain {
    errors: Vec<LoxError>,
}

#[derive(Clone, Debug)]
#[repr(u8)]
pub enum Op {
    OpAdd,
    OpConstant,
    OpDefineGlobal,
    OpPop,
    OpPrint,
    OpNil,
    OpNegate,
    OpNot,
    OpReturn,
    OpSubtract,
}

use Op::*;

impl From<&Op> for u8 {
    fn from(op: &Op) -> u8 {
        op.clone() as u8
    }
}

impl From<&u8> for Op {
    fn from(op: &u8) -> Op {
        match op {
            0 => OpDefineGlobal,
            1 => OpConstant,
            2 => OpPop,
            3 => OpPrint,
            4 => OpNil,
            5 => OpReturn,
            _ => panic!("Impossible op")
        }
    }
}


/// I just did this because Clippy told me to.
impl Default for VM {
    /// Create a new VM instance and set up its compiler
    /// which will produce the bytecode.
    ///
    /// Also fully initialize its memory, stack, and instruction
    /// pointer (which will point at the first instruction in
    /// the top-level-function that the script compiles to).
    fn default() -> Self {
        VM {
            stack: Vec::new(),
            memory: Memory::new(),
            ip: 0,
            frames: Vec::new(),
        }
    }
}

impl VM {
    /// Compile the script or line of code into bytecode, then
    /// execute the bytecode, all in the context of the VM that
    /// is set up with [new](#method.new).
    pub fn interpret(mut self, statement: &str) -> VM {
        let compiler = Compiler::new(&mut self);

        // Pass in the VM that calls the compiler so that the
        // compiler can swap itself out for a child compiler
        let function = compiler.compile(statement);

        match function {
            Ok(func) => {
                self.call(func, 0, 0);
                self.run();
            }
            Err(error) => println!("{}", error),
        }

        self
    }

    fn call(&mut self, closure: MemoryEntry, mem_entry: MemoryEntry, argc: usize) {
        let closure = self.memory.retrieve_mut(closure).as_function();
        if closure.arity != argc {
            let message = &format!("Expected {} arguments but got {}", closure.arity, argc);
            self.runtime_error(message);
        }

        if self.frames.len() >= MAX_FRAMES {
            self.runtime_error("Stack overflow.");
        }

        self.frames.push(CallFrame {
            closure: mem_entry,
            ip: 0,
            slots: 0,
            // slots: self.stack.len() - argc - 1,
        });
    }

    fn runtime_error(&self, message: &str) {
        todo!("emit runtime failure for: {}", message)
    }

    pub fn run(&mut self) {
        loop {
            let frame = self.frames.get(self.frames.len() - 1).unwrap();
            let op = self.closure(frame).chunk.get(frame.ip).unwrap().into();

            match op {
                OpAdd => {
                    let v1 = self.stack.pop().unwrap();
                    let v2 = self.stack.pop().unwrap();

                    if v1.is_string() && v2.is_string() {
                        let string = self.concatenate(v1.as_string(), v2.as_string());
                        self.stack.push(string)
                    } else if v1.is_number() && v2.is_number() {
                        let number = Number(v1.as_number() + v2.as_number());
                        self.stack.push(number)
                    } else {
                        self.runtime_error("Operands must be two numbers or two strings.")
                    }
                }
                OpConstant => todo!(),
                OpDefineGlobal => todo!(),
                OpPop => {
                    self.stack.pop();
                }
                OpPrint => {
                    let val = self.stack.get(self.stack.len() - 1).unwrap();

                    println!("{:?}", val);
                }
                OpNil => todo!(),
                OpNegate => todo!(),
                OpNot => todo!(),
                OpReturn => todo!(),
                OpSubtract => todo!(),
            }
        }
    }

    fn closure(&self, frame: &CallFrame) -> &Function {
        self.memory.retrieve(frame.ip).as_function()
    }

    pub fn concatenate(&self, str1: &str, str2: &str) -> Value {
        todo!(
            "concatenate two strings and intern the result ({}, {})",
            str1,
            str2
        )
    }
}

impl Display for LoxErrorChain {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl Display for LoxErrorSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl LoxErrorSpec {
    fn to_string(&self) -> String {
        format!("[line={}] {}", self.line, self.message)
    }
}

impl LoxError {
    fn to_string(&self) -> String {
        match self {
            LoxError::ScanError(s) => format!("ScanError: {}", s.to_string()),
            LoxError::ParseError(p) => format!("ParseError: {}", p.to_string()),
            LoxError::RuntimeError(r) => format!("RuntimeError: {}", r.to_string()),
        }
    }
}

impl LoxErrorChain {
    pub fn new() -> LoxErrorChain {
        LoxErrorChain { errors: Vec::new() }
    }

    pub fn register(&mut self, error: LoxError) {
        self.errors.push(error)
    }

    pub(crate) fn had_error(&self) -> bool {
        self.errors.len() > 0
    }

    pub(crate) fn print_all(&self) -> () {
        println!(
            "{}",
            self.errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }

    pub fn errors(&mut self) -> Vec<LoxError> {
        std::mem::replace(&mut self.errors, Vec::new())
    }
}

impl Error for LoxErrorSpec {}
impl Error for LoxErrorChain {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_run_hello_world_function() {
        VM::default().interpret("print \"Hello, world!\";");
    }
}
