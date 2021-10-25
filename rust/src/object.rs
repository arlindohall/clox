use std::collections::HashMap;

use crate::compiler::Function;

pub type MemoryEntry = usize;

#[derive(Debug)]
pub enum Object {
    _ObjBoundMethod(),
    _ObjClass(),
    _ObjClosure(),
    ObjFunction(Function),
    _ObjInstance(),
    _ObjNative(),
    ObjString(String),
    _ObjUpvalue(),
}

#[derive(Debug)]
pub struct Memory {
    count: usize,
    memory: HashMap<MemoryEntry, Object>,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            count: 0,
            memory: HashMap::new(),
        }
    }

    pub fn allocate(&mut self, object: Object) -> MemoryEntry {
        self.memory.insert(self.count, object);
        self.count += 1;

        self.count - 1
    }

    pub fn retrieve_mut(&mut self, index: MemoryEntry) -> &mut Object {
        self.memory.get_mut(&index).unwrap()
    }

    pub fn retrieve(&self, index: MemoryEntry) -> &Object {
        self.memory.get(&index).unwrap()
    }
}
impl Object {
    pub fn as_mut_function(&mut self) -> &mut Function {
        match self {
            Object::ObjFunction(f) => f,
            _ => panic!("Internal error: expected lox function type."),
        }
    }
    pub fn as_function(&self) -> &Function {
        match self {
            Object::ObjFunction(f) => f,
            _ => panic!("Internal error: expected lox function type."),
        }
    }
}
