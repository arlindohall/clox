
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
pub struct VM {}

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
        VM {}
    }

    /// Compile the script or line of code into bytecode, then
    /// execute the bytecode, all in the context of the VM that
    /// is set up with [new](#method.new).
    pub fn interpret(&self, statement: String) {
        println!("Running statement '{}'", statement);
    }
}