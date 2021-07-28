use crate::compiler::Compiler;
use crate::value::Value;

/// # Virtual Machine
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
/// let vm = VM::default();
/// vm.interpret("print \"Hello, world!\"".to_string());
/// ```
#[derive(Debug)]
pub struct VM {
    compilers: Vec<Compiler>,
    stack: Vec<Value>,
}

/// I just did this because Clippy told me to.
impl Default for VM {
    fn default() -> Self {
        VM::new()
    }
}

impl VM {
    /// Create a new VM instance and set up its compiler
    /// which will produce the bytecode.
    ///
    /// Also fully initialize its memory, stack, and instruction
    /// pointer (which will point at the first instruction in
    /// the top-level-function that the script compiles to).
    pub fn new() -> VM {
        VM {
            compilers: Vec::new(),
            stack: Vec::new(),
        }
    }

    /// Compile the script or line of code into bytecode, then
    /// execute the bytecode, all in the context of the VM that
    /// is set up with [new](#method.new).
    pub fn interpret(&mut self, statement: String) {
        let mut compiler = Compiler {};

        // Pass in the VM that calls the compiler so that the
        // compiler can swap itself out for a child compiler
        let function = compiler.compile(self, statement);

        println!("Compiled function {:?}", function);
    }
}
