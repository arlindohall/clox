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
            self.vm = self.vm.interpret(line?.as_str());
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

        self.vm.interpret(contents.as_str());

        Ok(())
    }
}
