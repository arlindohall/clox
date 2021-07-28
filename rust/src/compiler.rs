use crate::vm::VM;

/// # Compiler used for a single function or script
#[derive(Debug)]
pub struct Compiler {}

/// # A static function
///
/// This is the result of the [Compiler] method `compile`
/// because the top-level script is basically an immediately
/// invoked function.
///
/// The function holds onto compiled constant values and
/// anything it needs to be invoked as a closure will be
/// stored in a closure structure.
#[derive(Debug)]
pub struct Function {}

impl Compiler {
    pub fn compile(&mut self, vm: &mut VM, statement: String) -> Function {
        println!("VM and statement {:?}, {}", vm, statement);
        Function {}
    }
}
