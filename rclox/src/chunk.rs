
pub type ConstantPointer = usize;
pub struct Chunk {
    instructions: Vec<Op>,
    lines: Vec<usize>,
}

#[derive(Clone, Copy)]
pub enum Op {
    Constant(ConstantPointer),
    Nil,
    True,
    False,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Return,
}

/// # Chunk implementation
///
/// This one of the bigger divergences from the clox implementation so far, mostly because
/// we don't need to implement our own array-backed-list, we can just re-use Rust's.
///
/// The result is that we don't have many of the functions that the chunk implementation in
/// clox has, plus the ones we do have just dispatch to the list. We do still need to track
/// the line number for debug printing.
impl <'a> Chunk {
    pub fn new() -> Chunk {
        Chunk {
            instructions: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn instruction_at(&'a self, i: usize) -> &'a Op {
        &self.instructions[i]
    }
}