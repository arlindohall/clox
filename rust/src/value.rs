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
    Object(usize),
}

impl Value {
    pub fn is_string(&self) -> bool {
        todo!("check if value is a string")
    }

    pub fn as_string(&self) -> &String {
        todo!("convert a value to a string")
    }

    pub fn is_number(&self) -> bool {
        todo!("check if value is a number")
    }

    pub fn as_number(&self) -> f64 {
        todo!("convert a value to a number")
    }
}
