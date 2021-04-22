use crate::chunk::Chunk;


pub struct Compiler {}

impl Compiler {
    pub fn compile(&self, source: String) -> Result<Chunk, String> {
        Ok(Chunk::new())
    }
}