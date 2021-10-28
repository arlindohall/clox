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

    error_chain: LoxErrorChain,
}

#[derive(Debug)]
struct CallFrame {
    closure: MemoryEntry,
    ip: usize,
    slots: usize,
}

#[derive(Debug)]
pub enum LoxError {
    ScanError { line: usize, message: String },
    ParseError { line: usize, message: String },
    RuntimeError { line: usize, message: String },
}

#[derive(Debug)]
pub struct LoxErrorChain {
    errors: Vec<LoxError>,
}

#[derive(Clone, Debug)]
#[repr(u8)]
pub enum Op {
    OpAssert,
    OpAdd,
    OpAnd,
    OpConstant,
    OpDefineGlobal,
    OpDivide,
    OpEqual,
    OpGreater,
    OpGreaterEqual,
    OpOr,
    OpPop,
    OpMultiply,
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
        for v in &Op::values() {
            let val: u8 = v.into();
            if val == *op {
                return v.clone();
            }
        }

        panic!("Impossible op {}", op)
    }
}

impl Op {
    pub(crate) fn values() -> Vec<Op> {
        vec![
            OpAssert,
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
        ]
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
            memory: Memory::default(),
            ip: 0,
            frames: Vec::new(),
            error_chain: LoxErrorChain::default(),
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
                self.call(&func, crate::object::mem(0), 0);
                self.run();
            }
            Err(error) => println!("{}", error),
        }

        self
    }

    fn call(&mut self, closure: &MemoryEntry, mem_entry: MemoryEntry, argc: usize) {
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

    fn runtime_error(&mut self, message: &str) {
        let message = message.to_string();

        self.error_chain.register(LoxError::RuntimeError {
            // todo: track line numbers with bytecode
            message,
            line: self.line_number(),
        })
    }

    pub fn run(&mut self) {
        loop {
            let op = self.read_byte().into();

            match op {
                OpAssert => {
                    let val = self.stack.last().unwrap();

                    match val {
                        Boolean(false) | Nil => {
                            self.runtime_error("Failed assertion");
                            println!("{}", self.error_chain);
                            // todo: should this exit or just revert to top level?
                            std::process::exit(3);
                        }
                        _ => (),
                    }
                }
                OpAdd => {
                    let v1 = self.stack.pop().unwrap();
                    let v2 = self.stack.pop().unwrap();

                    if v1.is_string() && v2.is_string() {
                        let string = self
                            .concatenate(v1.as_string(&self.memory), v2.as_string(&self.memory));
                        self.stack.push(string)
                    } else if v1.is_number() && v2.is_number() {
                        let number = Number(v1.as_number() + v2.as_number());
                        self.stack.push(number)
                    } else {
                        self.runtime_error("Operands must be two numbers or two strings.")
                    }
                }
                OpConstant => {
                    let constant = self.read_constant().clone();
                    self.stack.push(constant);
                }
                OpDefineGlobal => todo!(),
                OpPop => {
                    self.stack.pop();
                }
                OpPrint => {
                    let val = self.stack.last().unwrap();

                    match val {
                        Nil => println!("nil"),
                        Number(n) => println!("{}", n),
                        Boolean(b) => println!("{}", b),
                        Object(ptr) => println!("{}", self.memory.retrieve(ptr)),
                    }
                }
                OpNil => {
                    self.stack.push(Nil)
                }
                OpNegate => {
                    let val = self.stack.pop().unwrap();

                    match val {
                        Number(n) => self.stack.push(Number(-n)),
                        _ => self.runtime_error("Cannot negate non-number."),
                    }
                }
                OpNot => {
                    let val = self.stack.pop().unwrap();

                    let opposite = matches!(val, Nil | Boolean(false));
                    self.stack.push(Boolean(opposite))
                }
                OpReturn => {
                    // todo: return from function, for now no-op
                    return;
                }
                OpSubtract => {
                    // Popped in reverse order they were pushed, expecting a-b
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    if let (Number(a), Number(b)) = (a, b) {
                        self.stack.push(Number(a-b))
                    } else {
                        self.runtime_error("Cannot subtract non-numbers")
                    }
                }
                OpAnd => {
                    let v1 = self.stack.pop().unwrap().as_boolean();
                    let v2 = self.stack.pop().unwrap().as_boolean();

                    self.stack.push(Boolean(v1 && v2))
                },
                OpEqual => todo!("compare two values for equality"),
                OpGreater => todo!("compare if value a is greater than value b"),
                OpGreaterEqual => todo!("compare if value a is greater or equal value b"),
                OpOr => todo!("logical or of two arguments"),
                OpMultiply => todo!("multiply two numbers"),
                OpDivide => todo!("divide two numbers"),
            }
        }
    }

    pub fn read_constant(&mut self) -> &Value {
        let index = *self.read_byte() as usize;
        let frame = self.frames.last().unwrap();
        self.closure(frame).chunk.constants.get(index).unwrap()
    }

    fn closure(&self, frame: &CallFrame) -> &Function {
        self.memory.retrieve(&frame.closure).as_function()
    }

    pub fn concatenate(&self, str1: &str, str2: &str) -> Value {
        todo!(
            "concatenate two strings and intern the result ({}, {})",
            str1,
            str2
        )
    }

    pub fn line_number(&self) -> usize {
        let frame = self.frames.last().unwrap();
        *self.closure(frame)
            .chunk
            .lines
            .get(frame.ip - 1)
            .unwrap()
    }

    pub fn read_byte(&mut self) -> &u8 {
        self.frames.last_mut().unwrap().ip += 1;
        let frame = self.frames.last().unwrap();
        self.closure(frame).chunk.code.get(frame.ip - 1).unwrap()
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

impl Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err_type = match self {
            LoxError::ScanError { .. } => "ScanError",
            LoxError::ParseError { .. } => "ParseError",
            LoxError::RuntimeError { .. } => "RuntimeError",
        };

        match self {
            LoxError::RuntimeError { message, line }
            | LoxError::ParseError { message, line }
            | LoxError::ScanError { message, line } => {
                write!(f, "[line={}] {}: {}", line, err_type, message)
            }
        }
    }
}

impl Default for LoxErrorChain {
    fn default() -> LoxErrorChain {
        LoxErrorChain { errors: Vec::new() }
    }
}

impl LoxErrorChain {
    pub fn register(&mut self, error: LoxError) {
        self.errors.push(error)
    }

    pub(crate) fn had_error(&self) -> bool {
        !self.errors.is_empty()
    }

    pub(crate) fn print_all(&self) {
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
        std::mem::take(&mut self.errors)
    }
}

impl Error for LoxErrorChain {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_run_hello_world_function() {
        VM::default().interpret("print \"Hello, world!\";");
    }

    #[test]
    fn test_assert_at_runtime() {
        VM::default().interpret("assert true;");
    }
}
