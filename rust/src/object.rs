use std::collections::HashMap;

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
    count: usize,
    memory: HashMap<usize, Object>,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            count: 0,
            memory: HashMap::new(),
        }
    }

    pub fn allocate(&mut self, object: Object) -> usize {
        let index = self.memory.len();
        self.memory.insert(self.count, object);
        self.count += 1;

        index
    }

    pub fn retrieve(&mut self, index: usize) -> &Object {
        self.memory.get(&index).unwrap()
    }
}
