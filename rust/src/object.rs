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
}

#[derive(Debug, Clone)]
enum UpvalueWrapper {
    Value(Value),
    StackPointer(usize),
    UpvaluePointer(MemoryEntry),
}

impl Copy for Upvalue {}
impl Copy for UpvalueWrapper {}

#[derive(Debug)]
pub struct Closure {
    pub(crate) function: MemoryEntry,
    pub(crate) upvalues: Vec<MemoryEntry>,
    pub(crate) enclosing: Option<MemoryEntry>,
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
            Object::String(s) => s,
            _ => panic!("Internal error: expected lox string type (this is a compiler bug)."),
        }
    }

    pub(crate) fn as_upvalue(&self) -> &Upvalue {
        match self {
            Object::Upvalue(u) => u,
            _ => {
                panic!("Internal error: expected lox closed stack value (this is a compiler bug).")
            }
        }
    }

    pub(crate) fn as_upvalue_mut(&mut self) -> &mut Upvalue {
        match self {
            Object::Upvalue(u) => u,
            _ => {
                panic!("Internal error: expected lox closed stack value (this is a compiler bug).")
            }
        }
    }
}

impl Upvalue {
    pub fn from_local(local: usize) -> Upvalue {
        Upvalue {
            value: UpvalueWrapper::StackPointer(local),
        }
    }

    pub fn from_enclosing(upvalue: MemoryEntry) -> Upvalue {
        Upvalue {
            value: UpvalueWrapper::UpvaluePointer(upvalue),
        }
    }

    pub fn is_closed(&self, vm: &VM) -> bool {
        match self.value {
            UpvalueWrapper::StackPointer(_) => false,
            UpvalueWrapper::Value(_) => true,
            UpvalueWrapper::UpvaluePointer(u) => vm.get_upvalue(u).is_closed(vm),
        }
    }

    pub fn close(&mut self, value: Value) {
        std::mem::swap(
            self,
            &mut Upvalue {
                value: UpvalueWrapper::Value(value),
            },
        );
    }

    pub fn is_root(&self) -> bool {
        !matches!(&self.value, UpvalueWrapper::UpvaluePointer(_))
    }

    pub fn is_local(&self) -> bool {
        matches!(&self.value, UpvalueWrapper::StackPointer(_))
    }

    pub fn get_local(&self) -> usize {
        if let UpvalueWrapper::StackPointer(p) = self.value {
            return p;
        }

        panic!("(Internal) Expected pointer to local variable, got closed value (this is a compiler bug).")
    }

    pub fn stack_index(&self, vm: &VM) -> usize {
        match self.value {
            UpvalueWrapper::StackPointer(s) => s,
            UpvalueWrapper::UpvaluePointer(u) => vm.get_upvalue(u).stack_index(vm),
            UpvalueWrapper::Value(_) => panic!("(Internal) Expected pointer, got closed value (this is a compiler bug)."),
        }
    }

    pub fn index(&self) -> usize {
        match self.value {
            UpvalueWrapper::StackPointer(p) => p,
            UpvalueWrapper::UpvaluePointer(_) => todo!(),
            UpvalueWrapper::Value(_) => panic!("(Internal) Found closed value while iterating through open values (this is a compiler bug)."),
        }
    }

    pub fn get_parent(&self) -> MemoryEntry {
        if let UpvalueWrapper::UpvaluePointer(p) = self.value {
            return p;
        }

        panic!("(Internal) Expected pointer to closed value, got stack pointer or closed value (this is a compiler bug).")
    }

    pub fn value(&self, vm: &VM) -> Value {
        match self.value {
            UpvalueWrapper::Value(v) => v,
            UpvalueWrapper::StackPointer(s) => vm.stack[s],
            UpvalueWrapper::UpvaluePointer(u) => vm.get_upvalue(u).value(vm),
        }
    }

    pub fn is_after(&self, stack_top: usize, vm: &VM) -> bool {
        match self.value {
            UpvalueWrapper::Value(_) => true,
            UpvalueWrapper::StackPointer(s) => s >= stack_top,
            UpvalueWrapper::UpvaluePointer(u) => vm.get_upvalue(u).is_after(stack_top, vm),
        }
    }
}
