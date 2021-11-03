use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;

use crate::compiler::{Compiler, Function};
use crate::debug::DebugTrace;
use crate::object::{Memory, MemoryEntry, Object};
use crate::value::Value;

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
    pub(crate) stack: Vec<Value>,
    globals: HashMap<String, Value>,

    // todo: replace all pub with pub(crate) where possible
    pub(crate) memory: Memory,

    /// Frames are public so the `debug` module can use them
    frames: Vec<CallFrame>,

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

pub mod op {
    #[repr(u8)]
    enum Op {
        Add,
        And,
        Assert,
        Constant,
        DefineGlobal,
        Divide,
        Equal,
        GetGlobal,
        GetLocal,
        Greater,
        GreaterEqual,
        Multiply,
        Negate,
        Nil,
        Not,
        Or,
        Pop,
        Print,
        Return,
        SetGlobal,
        SetLocal,
        Subtract,
    }
    use Op::*;

    pub const ADD: u8 = Add as u8;
    pub const AND: u8 = And as u8;
    pub const ASSERT: u8 = Assert as u8;
    pub const CONSTANT: u8 = Constant as u8;
    pub const DEFINE_GLOBAL: u8 = DefineGlobal as u8;
    pub const DIVIDE: u8 = Divide as u8;
    pub const EQUAL: u8 = Equal as u8;
    pub const GET_GLOBAL: u8 = GetGlobal as u8;
    pub const GET_LOCAL: u8 = GetLocal as u8;
    pub const GREATER: u8 = Greater as u8;
    pub const GREATER_EQUAL: u8 = GreaterEqual as u8;
    pub const MULTIPLY: u8 = Multiply as u8;
    pub const NEGATE: u8 = Negate as u8;
    pub const NIL: u8 = Nil as u8;
    pub const NOT: u8 = Not as u8;
    pub const OR: u8 = Or as u8;
    pub const POP: u8 = Pop as u8;
    pub const PRINT: u8 = Print as u8;
    pub const RETURN: u8 = Return as u8;
    pub const SET_GLOBAL: u8 = SetGlobal as u8;
    pub const SET_LOCAL: u8 = SetLocal as u8;
    pub const SUBTRACT: u8 = Subtract as u8;
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
            globals: HashMap::new(),
            memory: Memory::default(),
            frames: Vec::new(),
            error_chain: LoxErrorChain::default(),
        }
    }
}

impl VM {
    /// Compile the script or line of code into bytecode, then
    /// execute the bytecode, all in the context of the VM that
    /// is set up with [new](#method.new).
    pub fn interpret(mut self, statement: &str) -> Result<VM, (VM, LoxErrorChain)> {
        let compiler = Compiler::new(&mut self);

        // Pass in the VM that calls the compiler so that the
        // compiler can swap itself out for a child compiler
        let function = match compiler.compile(statement) {
            Ok(func) => func,
            Err(e) => {
                return Err((self, e));
            }
        };

        self.call(&function, 0);
        self.run();

        Ok(self)
    }

    fn call(&mut self, mem_entry: &MemoryEntry, argc: usize) {
        let closure = self.memory.retrieve_mut(mem_entry).as_function();
        if closure.arity != argc {
            let message = &format!("Expected {} arguments but got {}", closure.arity, argc);
            self.runtime_error(message);
        }

        if self.frames.len() >= MAX_FRAMES {
            self.runtime_error("Stack overflow.");
        }

        self.frames.push(CallFrame {
            closure: mem_entry.clone(),
            ip: 0,
            slots: 0,
            // slots: self.stack.len() - argc - 1,
        });
    }

    fn runtime_error(&mut self, message: &str) {
        let message = message.to_string();

        self.error_chain.register(LoxError::RuntimeError {
            message,
            line: self.line_number(),
        })
    }

    pub fn run(&mut self) {
        self.debug_trace_function();
        loop {
            self.debug_trace_instruction();
            let op = self.read_byte();

            match *op {
                op::ASSERT => {
                    let val = self.stack.last().unwrap();

                    match val {
                        Value::Boolean(false) | Value::Nil => {
                            self.runtime_error("Failed assertion");
                            // todo: should this exit or just revert to top level?
                            panic!("Assertion failed: {}", self.error_chain)
                        }
                        _ => (),
                    }
                }
                op::ADD => {
                    let v1 = self.stack.pop().unwrap();
                    let v2 = self.stack.pop().unwrap();

                    if self.is_string(&v1) && self.is_string(&v2) {
                        let v1 = v1.as_pointer();
                        let v2 = v2.as_pointer();

                        let v1 = self.memory.retrieve(&v1).as_string();
                        let v2 = self.memory.retrieve(&v2).as_string();
                        let string = self.concatenate(v1, v2);
                        self.stack.push(string)
                    } else if let (Some(n1), Some(n2)) = (v1.as_number(), v2.as_number()) {
                        self.stack.push(Value::Number(n1 + n2))
                    } else {
                        self.runtime_error("Operands must be two numbers or two strings.")
                    }
                }
                op::CONSTANT => {
                    let constant = self.read_constant().clone();
                    self.stack.push(constant);
                }
                op::DEFINE_GLOBAL => {
                    let name = *self.read_byte();
                    let name = self
                        .current_closure()
                        .chunk
                        .constants
                        .get(name as usize)
                        .unwrap()
                        .as_pointer();
                    let name = self.memory.retrieve(&name).as_string().clone();
                    self.globals.insert(name, self.stack.pop().unwrap());
                }
                op::GET_GLOBAL => {
                    let name = *self.read_byte() as usize;
                    let name = self
                        .current_closure()
                        .chunk
                        .constants
                        .get(name)
                        .unwrap()
                        .as_pointer();
                    let name = self.memory.retrieve(&name).as_string();

                    self.stack.push(self.globals.get(name).unwrap().clone())
                }
                op::SET_GLOBAL => {
                    todo!("update global constants table")
                }
                op::GET_LOCAL => {
                    let base = self.frames.last().unwrap().slots;
                    let offset = *self.read_byte() as usize;
                    let index = base + offset - 1;

                    self.stack.push(self.stack.get(index).unwrap().clone())
                }
                op::SET_LOCAL => {
                    let base = self.frames.last().unwrap().slots;
                    let offset = *self.read_byte() as usize;
                    let index = base + offset - 1;

                    let new_value = self.stack.pop().unwrap();
                    self.stack.insert(index, new_value)
                }
                op::POP => {
                    self.stack.pop();
                }
                op::PRINT => {
                    let val = self.stack.last().unwrap();

                    match val {
                        Value::Nil => println!("nil"),
                        Value::Number(n) => println!("{}", n),
                        Value::Boolean(b) => println!("{}", b),
                        Value::Object(ptr) => println!("{}", self.memory.retrieve(ptr)),
                    }
                }
                op::NIL => self.stack.push(Value::Nil),
                op::NEGATE => {
                    let val = self.stack.pop().unwrap();

                    match val {
                        Value::Number(n) => self.stack.push(Value::Number(-n)),
                        _ => self.runtime_error("Cannot negate non-number."),
                    }
                }
                op::NOT => {
                    let val = self.stack.pop().unwrap();

                    let opposite = matches!(val, Value::Nil | Value::Boolean(false));
                    self.stack.push(Value::Boolean(opposite))
                }
                op::RETURN => {
                    // todo: return from function, for now no-op
                    return;
                }
                op::SUBTRACT => {
                    // Popped in reverse order they were pushed, expecting a-b
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        self.stack.push(Value::Number(a - b))
                    } else {
                        self.runtime_error("Cannot subtract non-numbers")
                    }
                }
                op::MULTIPLY => {
                    // Popped in reverse order they were pushed, expecting a-b
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        self.stack.push(Value::Number(a * b))
                    } else {
                        self.runtime_error("Cannot multiply non-numbers")
                    }
                }
                op::DIVIDE => {
                    // Popped in reverse order they were pushed, expecting a-b
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        self.stack.push(Value::Number(a / b))
                    } else {
                        self.runtime_error("Cannot divide non-numbers")
                    }
                }
                op::AND => {
                    let v1 = self.stack.pop().unwrap().as_boolean();
                    let v2 = self.stack.pop().unwrap().as_boolean();

                    self.stack.push(Value::Boolean(v1 && v2))
                }
                op::EQUAL => {
                    let v1 = self.stack.pop().unwrap();
                    let v2 = self.stack.pop().unwrap();

                    let equal = match (v1, v2) {
                        (Value::Number(n1), Value::Number(n2)) => (n2 - n1).abs() <= f64::EPSILON,
                        (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
                        (Value::Nil, Value::Nil) => true,
                        (Value::Object(o1), Value::Object(o2)) => o1 == o2,
                        _ => false,
                    };

                    self.stack.push(Value::Boolean(equal))
                }
                op::GREATER => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    if let (Some(a), Some(b)) = (a.as_number(), b.as_number()) {
                        self.stack.push(Value::Boolean(a > b))
                    } else {
                        self.runtime_error("Cannot compare two non-numbers.")
                    }
                }
                op::GREATER_EQUAL => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    if let (Some(a), Some(b)) = (a.as_number(), b.as_number()) {
                        self.stack.push(Value::Boolean(a >= b))
                    } else {
                        self.runtime_error("Cannot compare two non-numbers.")
                    }
                }
                op::OR => {
                    let v1 = self.stack.pop().unwrap().as_boolean();
                    let v2 = self.stack.pop().unwrap().as_boolean();

                    self.stack.push(Value::Boolean(v1 || v2))
                }
                b => panic!("Invalid bytecode {}", b),
            }
        }
    }

    pub fn read_constant(&mut self) -> &Value {
        let index = *self.read_byte() as usize;
        self.current_closure().chunk.constants.get(index).unwrap()
    }

    pub(crate) fn current_closure(&self) -> &Function {
        let frame = self.frames.last().unwrap();
        self.memory.retrieve(&frame.closure).as_function()
    }

    pub(crate) fn ip(&self) -> usize {
        self.frames.last().unwrap().ip
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
        *self
            .current_closure()
            .chunk
            .lines
            .get(frame.ip - 1)
            .unwrap()
    }

    pub fn read_byte(&mut self) -> &u8 {
        let ip = self.ip();
        self.frames.last_mut().unwrap().ip += 1;

        self.current_closure().chunk.code.get(ip).unwrap()
    }

    pub fn is_string(&self, value: &Value) -> bool {
        match value {
            Value::Object(ptr) => {
                matches!(self.memory.retrieve(ptr), Object::String(_))
            }
            _ => false,
        }
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

    fn run(statement: &str) {
        unsafe {
            // Debug print in case the test fails, makes debugging easier
            crate::debug::DEBUG_PRINT_CODE = true;
            crate::debug::DEBUG_TRACE_EXECUTION = true;
        }
        match VM::default().interpret(statement) {
            Ok(_) => (),
            Err((_, e)) => {
                println!("Error interpreting statement {}", e);
                panic!()
            }
        }
    }

    #[test]
    fn run_hello_world_function() {
        run("print \"Hello, world!\";");
    }

    #[test]
    fn assert_at_runtime() {
        run("assert true;");
    }

    #[test]
    #[should_panic]
    fn assert_false() {
        run("assert false;");
    }

    #[test]
    fn compare_two_numbers() {
        run("assert 1 == 1;")
    }

    #[test]
    fn add_two_numbers() {
        run("assert 1 + 1 == 2;")
    }

    #[test]
    fn greater_than_numbers() {
        run("assert 2 > 1;
            assert ! (1 > 1);
            assert ! (0 > 1);");
    }

    #[test]
    fn greater_than_equal_numbers() {
        run("assert 2 >= 1;
            assert 1 >= 1;
            assert ! (0 > 1);");
    }

    #[test]
    fn less_than_numbers() {
        run("assert ! (2 < 1);
            assert ! (1 < 1);
            assert 0 < 1;");
    }

    #[test]
    fn less_than_equal_numbers() {
        run("assert ! (2 <= 1);
            assert 1 <= 1;
            assert 0 <= 1;");
    }

    #[test]
    fn multiply_and_divide() {
        run("assert 2 * 2 == 4;
            assert 10 / 5 == 2;");
    }

    #[test]
    fn define_and_reference_global_variable() {
        run("var x = 10;
            assert x == 10;");
    }

    #[test]
    fn local_variables_block_scoping() {
        run("
            var x = 10;
            {
                var x = 20;
                assert x == 20;
            }
            assert x == 10;
            {
                var x = 30;
                assert x == 30;
            }
        ");
    }

    #[test]
    fn local_variables_get_and_set() {
        run("
            var x = 10;
            {
                var x;
                assert x == nil;

                x = 20;
                assert x == 20;
            }
            assert x == 10;");
    }
}
