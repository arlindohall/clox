use std::{collections::HashMap, fmt::Display};

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
    _BoundMethod(),
    _Class(),
    _Closure(),
    Function(Box<Function>),
    _Instance(),
    _Native(),
    String(Box<String>),
    _Upvalue(),
}

#[derive(Debug)]
pub struct Memory {
    count: usize,
    memory: HashMap<MemoryEntry, Object>,
    strings: HashMap<Box<String>, MemoryEntry>,
}

impl Default for Memory {
    fn default() -> Memory {
        Memory {
            count: 0,
            memory: HashMap::new(),
            strings: HashMap::new(),
        }
    }
}

impl Memory {
    pub fn allocate(&mut self, object: Object) -> MemoryEntry {
        if let Object::String(s) = object {
            self.intern(s)
        } else {
            self.insert(object)
        }
    }

    pub fn retrieve_mut(&mut self, index: &MemoryEntry) -> &mut Object {
        self.memory.get_mut(index).unwrap()
    }

    pub fn retrieve(&self, index: &MemoryEntry) -> &Object {
        self.memory.get(index).unwrap()
    }

    fn intern(&mut self, string: Box<String>) -> MemoryEntry {
        match self.strings.get(&string) {
            Some(m_loc) => m_loc.clone(),
            None => self.insert(Object::String(string)),
        }
    }

    fn insert(&mut self, object: Object) -> MemoryEntry {
        let m_loc = mem(self.count);
        self.memory.insert(m_loc.clone(), object);
        self.count += 1;

        m_loc
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::_BoundMethod() => todo!("display a bound method"),
            Object::_Class() => todo!("display a class"),
            Object::_Closure() => todo!("display a closure"),
            Object::Function(func) => write!(f, "fn<{}>", func.name),
            Object::_Instance() => todo!("display an instance"),
            Object::_Native() => todo!("display a native function"),
            Object::String(s) => write!(f, "{}", s),
            Object::_Upvalue() => todo!("display an upvalue"),
        }
    }
}

impl Object {
    pub fn as_mut_function(&mut self) -> &mut Function {
        match self {
            Object::Function(f) => f,
            _ => panic!("Internal error: expected lox function type."),
        }
    }
    pub fn as_function(&self) -> &Function {
        match self {
            Object::Function(f) => f,
            _ => panic!("Internal error: expected lox function type."),
        }
    }

    pub(crate) fn as_string(&self) -> &String {
        todo!()
    }
}
