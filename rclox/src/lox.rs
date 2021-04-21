use std::{error::Error, fs::File, io::{BufRead, Read}, process::exit};

use crate::vm::Vm;

pub struct Lox {
    vm: Vm,
}

/// `Clox` uses an enum for error types, we use a Rust enum
pub enum LoxError {
    CompileError {},
    RuntimeError {},
}

impl LoxError {
    pub fn to_string(&self) -> String {
        "".to_string()
    }
}

/// # Lox functions
///
/// These are procedures related to the VM. In `clox`, we use a static variable to
/// represent the VM, but here we'll use an explicit variable, so these helper functions
/// don't live in the main module.
impl Lox {

    pub fn new() -> Lox {
        Lox {
            vm: Vm::new(),
        }
    }

    pub fn repl(&self) -> Result<(), Box<dyn Error>> {
        let lock = std::io::stdin();
        let lock = lock.lock();

        print!("> ");
        for line in lock.lines() {
            match self.vm.interpret(line?) {
                Ok(_) => (),
                Err(e) => println!("Error while interpreting line: {}", e.to_string())
            }
            print!("> ");
        }

        Ok(())
    }

    pub fn run_file(&self, file_name: &String) -> Result<(), Box<dyn Error>> {
        let mut source = String::new();
        File::open(file_name)?.read_to_string(&mut source)?;

        match self.vm.interpret(source) {
            Ok(_) => (),
            Err(LoxError::CompileError { .. }) => exit(65),
            Err(LoxError::RuntimeError { .. }) => exit(70),
        }
        Ok(())
    }
}