use std::collections::{HashMap, HashSet};

use crate::compiler::Function;

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct MemoryEntry {
    location: usize,
}

pub fn mem(location: usize) -> MemoryEntry {
    MemoryEntry { location }
}

impl Eq for MemoryEntry {}

#[derive(Debug)]
pub enum Object {
    _ObjBoundMethod(),
    _ObjClass(),
    _ObjClosure(),
    ObjFunction(Box<Function>),
    _ObjInstance(),
    _ObjNative(),
    ObjString(Box<String>),
    _ObjUpvalue(),
}

#[derive(Debug)]
pub struct Memory {
    count: usize,
    memory: HashMap<MemoryEntry, Object>,
    strings: HashSet<String>,
}

impl Default for Memory {
    fn default() -> Memory {
        Memory {
            count: 0,
            memory: HashMap::new(),
            strings: HashSet::new(),
        }
    }
}

impl Memory {
    pub fn allocate(&mut self, object: Object) -> MemoryEntry {
        let m_loc = mem(self.count);
        self.memory.insert(m_loc.clone(), object);
        self.count += 1;

        m_loc
    }

    pub fn retrieve_mut(&mut self, index: &MemoryEntry) -> &mut Object {
        self.memory.get_mut(index).unwrap()
    }

    pub fn retrieve(&self, index: &MemoryEntry) -> &Object {
        self.memory.get(index).unwrap()
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

    pub(crate) fn as_string(&self) -> &String {
        todo!()
    }
}
