use crate::object::{Memory, MemoryEntry};

/// Internal Lox value representation
///
/// This enum represents all the kinds of values that can
/// go on the Lox runtime stack. The values can also be
/// created at compile time (esp. functions and constants).
/// The main use, though, is pushing and poping values to the
/// stack.
#[derive(Debug)]
pub enum Value {
    Number(f64),
    _Boolean(bool),
    Object(MemoryEntry),
}

impl Value {
    pub fn is_string(&self) -> bool {
        todo!("check if value is a string")
    }

    pub fn as_string<'a>(&self, mem: &'a Memory) -> &'a String {
        if let Value::Object(ptr) = self {
            mem.retrieve(ptr).as_string()
        } else {
            panic!("Internall lox error (expected string type), this is a bug.")
        }
    }

    pub fn is_number(&self) -> bool {
        todo!("check if value is a number")
    }

    pub fn as_number(&self) -> f64 {
        todo!("convert a value to a number")
    }
}
