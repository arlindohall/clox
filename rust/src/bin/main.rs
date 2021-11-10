use std::{
    error::Error,
    fs::File,
    io::{BufRead, Read, Write},
};

use loxvm::vm::VM;

/// Entry point for the lox interpreter
///
/// This function plust the Lox struct handles most of the IO
/// which leaves the rest of the implementaiton to deal with the
/// langauge, and makes the rest of the VM, compiler, and related
/// constructs easier to test.
fn main() -> Result<(), Box<dyn Error>> {
    let lox = Lox { vm: VM::default() };

    // todo: allow caller to set debug output here
    // unsafe {
    //     loxvm::compiler::DEBUG_PRINT_CODE = loxvm::compiler::DebugOutput::Table;
    // }
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() == 1 {
        lox.repl()
    } else if args.len() == 2 {
        lox.run_file(&args[1])
    } else {
        println!("Usage: clox [path]");
        Ok(())
    }
}

/// The top-level language, including the IO logic for
/// the [REPL](#method.repl) and script interpreter.
/// It has functions for running the REPL aand files.
struct Lox {
    vm: VM,
}

/// A private helper that produces the prompt in the REPL
fn prompt() -> Result<(), std::io::Error> {
    print!("> ");
    std::io::stdout().flush()
}

impl Lox {
    /// Just read a line at a time from stdin. If an error
    /// happens either while reading a line or while executing
    /// it, then we'll just fail and exit immediately. Otherwise
    /// we put the prompt up and read another line.
    ///
    /// We rely on the [VM](../loxvm/vm/struct.VM.html) struct to
    /// track any state.
    fn repl(mut self) -> Result<(), Box<dyn Error>> {
        let stdin = std::io::stdin();
        let lock = stdin.lock();

        prompt()?;
        for line in lock.lines() {
            self.vm = match self.vm.interpret(line?.as_str()) {
                // todo: have interpret return a value and print that value...
                Ok((vm, value)) => {
                    println!("{}", value);
                    vm
                }
                Err(vm) => {
                    vm.print_errors();
                    vm
                }
            };
            prompt()?;
        }

        Ok(())
    }

    /// This works almost exactly like the repl, except
    /// the interpreter will end up stripping all whitespace,
    /// which means newlines are ignored and you can use
    /// multiline statements in scripts.
    fn run_file(self, file_name: &str) -> Result<(), Box<dyn Error>> {
        let mut contents = String::new();
        let mut file = File::open(file_name)?;

        file.read_to_string(&mut contents)?;

        if let Err(vm) = self.vm.interpret(contents.as_str()) {
            vm.print_errors();
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {

    use super::*;

    macro_rules! test_script {
        ($name:ident, $filename:literal) => {
            #[test]
            fn $name() -> Result<(), std::io::Error> {
                unsafe {
                    loxvm::debug::DEBUG_PRINT_CODE = true;
                    loxvm::debug::DEBUG_TRACE_EXECUTION = true;
                }
                let mut contents = String::new();

                let mut filename = String::from("assets/");
                filename.push_str($filename);
                filename.push_str(".lox");

                let mut test_script = File::open(filename)?;

                test_script.read_to_string(&mut contents)?;

                match VM::default().interpret(contents.as_str()) {
                    Ok(_) => (),
                    Err(vm) => {
                        println!("Failing test because of error");
                        vm.print_errors();
                        panic!()
                    }
                }

                Ok(())
            }
        };
    }

    test_script! { run_file_var, "var" }
    test_script! { run_file_block_scope, "block-scope"}
    test_script! { run_file_expr, "expr" }
    test_script! { run_file_print, "print" }
    test_script! { run_file_simple, "simple" }
    test_script! { run_file_if_and_while, "if-and-while" }
    test_script! { run_file_crazy_closure, "crazy-closure" }
}
