use std::fmt::Display;

use crate::object::{Memory, MemoryEntry};

/// Internal Lox value representation
///
/// This enum represents all the kinds of values that can
/// go on the Lox runtime stack. The values can also be
/// created at compile time (esp. functions and constants).
/// The main use, though, is pushing and poping values to the
/// stack.
#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Object(MemoryEntry),
    Nil,
}

impl Value {
    pub fn as_string<'a>(&self, mem: &'a Memory) -> &'a String {
        if let Value::Object(ptr) = self {
            mem.retrieve(ptr).as_string()
        } else {
            panic!("Internall lox error (expected string type), this is a bug.")
        }
    }

    pub fn as_number(&self) -> Option<f64> {
        match self {
            Value::Number(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_boolean(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Object(_) => write!(f, "<ptr>"),
            Value::Nil => write!(f, "nil"),
        }
    }
}
