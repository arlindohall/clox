use std::error::Error;
use std::fmt::Display;

use crate::compiler::Compiler;
use crate::object::Memory;
use crate::value::Value;

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
/// // todo
/// // vm.interpret("print \"Hello, world!\"");
/// ```
#[derive(Debug)]
pub struct VM {
    stack: Vec<Value>,

    pub memory: Memory,
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

#[repr(u8)]
pub enum Op {
    OpAdd,
    OpConstant,
    OpDefineGlobal,
    OpPop,
    OpPrint,
    OpNil,
    OpReturn,
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
            Ok(func) => println!("Compiled function {:?}", func),
            Err(error) => println!("{}", error),
        }

        self
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
