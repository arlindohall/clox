use std::{collections::HashMap, fmt::Display};

use crate::{compiler::Function, value::Value, vm::VM};

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct MemoryEntry {
    location: usize,
}

impl Copy for MemoryEntry {}

pub fn mem(location: usize) -> MemoryEntry {
    MemoryEntry { location }
}

impl Eq for MemoryEntry {}

#[derive(Debug)]
pub enum Object {
    _BoundMethod(),
    _Class(),
    Closure(Box<Closure>),
    Function(Box<Function>),
    _Instance(),
    _Native(),
    String(Box<String>),
    Upvalue(Box<Upvalue>),
}

#[derive(Debug, Clone)]
pub struct Upvalue {
    value: UpvalueWrapper,
    pub closure: (MemoryEntry, usize),
}

#[derive(Debug, Clone)]
enum UpvalueWrapper {
    Value(Value),
    StackPointer(usize),
    UpvaluePointer(usize),
}

impl Copy for Upvalue {}
impl Copy for UpvalueWrapper {}

#[derive(Debug)]
pub struct Closure {
    pub(crate) function: MemoryEntry,
    pub(crate) upvalues: Vec<Upvalue>,
}

#[derive(Debug)]
pub struct Memory {
    count: usize,
    memory: HashMap<MemoryEntry, Object>,
    strings: HashMap<String, MemoryEntry>,
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

    pub fn is_valid(&self, ptr: MemoryEntry) -> bool {
        self.memory.len() > ptr.location
    }

    pub fn retrieve_mut(&mut self, index: &MemoryEntry) -> Option<&mut Object> {
        self.memory.get_mut(index)
    }

    pub fn retrieve(&self, index: &MemoryEntry) -> Option<&Object> {
        self.memory.get(index)
    }

    pub fn intern(&mut self, string: Box<String>) -> MemoryEntry {
        match self.strings.get(&*string) {
            Some(m_loc) => *m_loc,
            None => {
                let m_loc = self.insert(Object::String(string.clone()));
                self.strings.insert(*string, m_loc);

                m_loc
            }
        }
    }

    fn insert(&mut self, object: Object) -> MemoryEntry {
        let m_loc = mem(self.count);
        self.memory.insert(m_loc, object);
        self.count += 1;

        m_loc
    }
}

impl Display for MemoryEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:010}", self.location)
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::_BoundMethod() => todo!("display a bound method"),
            Object::_Class() => todo!("display a class"),
            Object::Function(func) => write!(f, "fn<{}>", func.name),
            Object::_Instance() => todo!("display an instance"),
            Object::_Native() => todo!("display a native function"),
            Object::String(s) => write!(f, "\"{}\"", s),
            Object::Upvalue(_) => panic!("(Internal) cannot display closed stack object"),
            Object::Closure(_) => panic!("(Internal) cannot display closure object"),
        }
    }
}

impl Object {
    pub fn as_function_mut(&mut self) -> &mut Function {
        match self {
            Object::Function(f) => f,
            _ => panic!("Internal error: expected lox function type (this is a compiler bug)."),
        }
    }

    pub fn as_function(&self) -> &Function {
        match self {
            Object::Function(f) => f,
            _ => panic!("Internal error: expected lox function type (this is a compiler bug)."),
        }
    }

    pub fn as_closure_mut(&mut self) -> &mut Closure {
        match self {
            Object::Closure(c) => c,
            _ => panic!("Internal error: expected lox closure type (this is a compiler bug)."),
        }
    }

    pub fn as_closure(&self) -> &Closure {
        match self {
            Object::Closure(c) => c,
            _ => panic!("Internal error: expected lox closure type (this is a compiler bug)."),
        }
    }

    pub(crate) fn as_string(&self) -> &String {
        match self {
            Object::String(s) => s.as_ref(),
            _ => panic!("Internal error: expected lox string type (this is a compiler bug)."),
        }
    }
}

impl Upvalue {

    pub fn from_local(local: usize) -> Upvalue {
        Upvalue {
            value: UpvalueWrapper::StackPointer(local),
            closure: (mem(0), 0),
        }
    }

    pub fn from_closed(upvalue: usize) -> Upvalue {
        Upvalue {
            value: UpvalueWrapper::UpvaluePointer(upvalue),
            closure: (mem(0), 0),
        }
    }

    pub fn should_close(&self, current_stack: usize, vm: &VM) -> bool {
        match self.value {
            UpvalueWrapper::StackPointer(p) => p <= current_stack,
            UpvalueWrapper::UpvaluePointer(v) => vm.get_upvalue(v as u8).should_close(current_stack, vm),
            UpvalueWrapper::Value(_) => panic!("(Internal) value already closed is still open in VM."),
        }
    }

    pub fn close(&mut self, value: Value) {
        std::mem::swap(self, &mut Upvalue {
            value: UpvalueWrapper::Value(value),
            closure: self.closure,
        });
    }

    pub fn value(&self, vm: &VM) -> Value {
        match self.value {
            UpvalueWrapper::Value(v) => v,
            UpvalueWrapper::StackPointer(p) => *vm.stack.get(p).unwrap(),
            UpvalueWrapper::UpvaluePointer(p) => vm.get_upvalue(p as u8).value(vm),
        }
    }
}
