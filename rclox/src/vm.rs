use crate::lox::LoxError;


pub struct Vm {}

impl Vm {
    pub fn new() -> Vm {
        Vm {}
    }

    pub fn interpret(&self, body: String) -> Result<(), LoxError> {
        Ok(())
    }
}

