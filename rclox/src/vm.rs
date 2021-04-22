use crate::{chunk::{Chunk, ConstantPointer, Op}, compiler::Compiler, lox::LoxError, value::Value};


/// # Vm data structure
///
/// This is more or less a clone of the VM struct. The biggest difference is
/// we don't keep a pointer to the top of the stack, but just track the index
/// instead. The pointer math is the same, but we use Rust's standard Vec
/// implementation rather than writing a bunch of resizing logic.
pub struct Vm {
    compiler: Compiler,
    chunk: Chunk,
    ip: usize,
    stack: Vec<ConstantPointer>,
    stack_top: usize,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            compiler: Compiler {},
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::new(),
            stack_top: 0,
        }
    }

    pub fn interpret(&mut self, source: String) -> Result<(), LoxError> {
        match self.compiler.compile(source) {
            Ok(chunk) => {
                self.chunk = chunk;
                // TODO: is this right? Not sure if the chunk should
                // include a pointer of its own, or maybe because
                // this is a new chunk it will definitely point to
                // the start
                self.ip = 0;
                self.run()
            }
            Err(message) => Err(LoxError::CompileError {
                message
            }),
        }
    }

    pub fn push(&mut self, value: ConstantPointer) {
        self.stack.push(value)
    }

    pub fn peek(&self, distance: usize) -> ConstantPointer {
        self.stack[self.stack_top - 1 - distance]
    }

    fn run(&mut self) -> Result<(), LoxError> {
        let instruction = self.read_byte();
        match instruction {
            Op::Constant(value) => {
                self.push(value.clone());
            }
            Op::Nil => self.push(),
            _ => panic!()
        }

        Ok(())
    }

    fn read_byte(&mut self) -> Op {
        self.ip += 1;
        *self.chunk.instruction_at(self.ip)
    }

    fn read_constant(&self) -> &Value {
        todo!()
    }
}

