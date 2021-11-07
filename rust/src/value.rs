use std::fmt::Display;

use crate::object::MemoryEntry;

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
    pub fn as_pointer(&self) -> MemoryEntry {
        if let Value::Object(ptr) = self {
            *ptr
        } else {
            panic!("Internal lox error (expected object), this is a bug.")
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

    /// Compare primitives or the real content of two objects.
    ///
    /// Since we don't have access to the runtime value of the object, i.e. we need
    /// to peek into memory to inspect Objects, we have to trust the caller to do
    /// that comparison. The logic for Object comparison will be relegated to the
    /// Object and VM impls.
    pub fn loose_equals(&self, value: &Value) -> Result<bool, (MemoryEntry, MemoryEntry)> {
        match (self, value) {
            (Value::Object(o1), Value::Object(o2)) => Err((*o1, *o2)),
            _ => Ok(self.strict_equals(value)),
        }
    }

    /// Compare two primitives or the memory location of two objects.
    pub fn strict_equals(&self, value: &Value) -> bool {
        match (self, value) {
            (Value::Number(n1), Value::Number(n2)) => (n2 - n1).abs() <= f64::EPSILON,
            (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
            (Value::Nil, Value::Nil) => true,
            (Value::Object(o1), Value::Object(o2)) => o1 == o2,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Object(p) => write!(f, "<ptr {}>", p),
            Value::Nil => write!(f, "nil"),
        }
    }
}
