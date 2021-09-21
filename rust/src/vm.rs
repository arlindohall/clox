use crate::compiler::Compiler;
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
pub struct VM<'a> {
    compilers: Vec<Compiler<'a>>,
    stack: Vec<Value>,
}

#[repr(u8)]
pub enum Op {
    OpDefineGlobal,
    OpNil,
    OpReturn,
}

/// I just did this because Clippy told me to.
impl<'a> Default for VM<'a> {
    /// Create a new VM instance and set up its compiler
    /// which will produce the bytecode.
    ///
    /// Also fully initialize its memory, stack, and instruction
    /// pointer (which will point at the first instruction in
    /// the top-level-function that the script compiles to).
    fn default() -> Self {
        VM {
            compilers: Vec::new(),
            stack: Vec::new(),
        }
    }
}

impl<'a> VM<'a> {
    /// Compile the script or line of code into bytecode, then
    /// execute the bytecode, all in the context of the VM that
    /// is set up with [new](#method.new).
    pub fn interpret(&mut self, statement: &str) {
        let compiler = Compiler::new(self);

        // Pass in the VM that calls the compiler so that the
        // compiler can swap itself out for a child compiler
        let function = compiler.compile(statement);

        match function {
            Ok(func) => println!("Compiled function {:?}", func),
            Err(error) => println!("Whoopsie-daisy {}", error),
        }
    }
}
