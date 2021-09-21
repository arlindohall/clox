/// Internal Lox value representation
///
/// This enum represents all the kinds of values that can
/// go on the Lox runtime stack. The values can also be
/// created at compile time (esp. functions and constants).
/// The main use, though, is pushing and poping values to the
/// stack.
#[derive(Debug)]
pub enum Value {
    _Number(f64),
    _Boolean(bool),
    Object(usize),
}
