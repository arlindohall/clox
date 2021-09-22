#[derive(Debug)]
pub enum Object {
    _ObjBoundMethod(),
    _ObjClass(),
    _ObjClosure(),
    _ObjFunction(),
    _ObjInstance(),
    _ObjNative(),
    ObjString(String),
    _ObjUpvalue(),
}

#[derive(Debug)]
pub struct Memory {
    memory: Vec<Object>,
}

impl Memory {
    pub fn new() -> Memory {
        Memory { memory: Vec::new() }
    }

    pub fn allocate(&mut self, object: Object) -> usize {
        let index = self.memory.len();
        self.memory.push(object);

        index
    }
}
