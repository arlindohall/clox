use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;

use crate::compiler::Compiler;
use crate::compiler::Function;
use crate::compiler::Parser;
use crate::debug::DebugTrace;
use crate::object::Upvalue;
use crate::object::{Closure, Memory, MemoryEntry, Object};
use crate::scanner::Scanner;
use crate::value::Value;

const MAX_FRAMES: usize = 265;

pub const STACK_ERROR: &str = "(Internal) tried to access invalid stack position.";
pub const CONSTANT_ERROR: &str = "(Internal) tried to access invalid constant.";
pub const GLOBAL_ERROR: &str = "Fatal: tried to access non-existent global variable.";
pub const MEMORY_ERROR: &str = "(Internal) invalid memory access.";
pub const STACK_UNDERFLOW: &str = "(Internal) tried to access invalid stack frame.";
pub const STACK_OVERFLOW: &str = "Stack overflow.";

/// Lox Virtual Machine.
///
/// This struct contains the VM runtime properties that
/// track the stack and objects.
///
/// In the C version of the interpreter, the VM is a global
/// variable, and it contains all of the values used by the
/// interpreter at runtime. This struct contains those same
/// values, but is initialized by the caller.
///
/// # Example Use
///
/// ```
/// # use loxvm::vm::VM;
/// let mut vm = VM::default();
/// vm.interpret("print \"Hello, world!\"");
/// ```
#[derive(Debug)]
pub struct VM {
    pub(crate) stack: Vec<Value>,
    globals: HashMap<String, Value>,

    // todo: replace all pub with pub(crate) where possible
    pub(crate) memory: Memory,

    /// Frames are public so the `debug` module can use them
    frames: Vec<CallFrame>,
    open_upvalues: Vec<MemoryEntry>,

    error_chain: LoxErrorChain,
}

#[derive(Debug)]
struct CallFrame {
    closure: MemoryEntry,
    ip: usize,
    slots: usize,
}

#[derive(Debug)]
pub enum LoxError {
    ScanError { line: usize, message: String },
    ParseError { line: usize, message: String },
    RuntimeError { line: usize, message: String },
}

#[derive(Debug)]
pub struct LoxErrorChain {
    errors: Vec<LoxError>,
}

macro_rules! pop_stack {
    ($vm:ident) => {
        match $vm.stack.pop() {
            Some(val) => val,
            None => $vm.fatal_error(STACK_ERROR),
        }
    };
}

macro_rules! get_stack {
    ($vm:ident, $index:expr) => {
        match $vm.stack.get($index) {
            Some(val) => val,
            None => $vm.fatal_error(STACK_ERROR),
        }
    };
}

macro_rules! peek_stack {
    ($vm:ident) => {
        match $vm.stack.last() {
            Some(val) => val,
            None => $vm.fatal_error(STACK_ERROR),
        }
    };
}

macro_rules! pop_frame {
    ($vm:ident) => {
        match $vm.frames.pop() {
            Some(frame) => frame,
            None => $vm.fatal_error(STACK_UNDERFLOW),
        }
    };
}

macro_rules! get_frame {
    ($vm:ident) => {
        match $vm.frames.last() {
            Some(frame) => frame,
            None => $vm.fatal_error(STACK_UNDERFLOW),
        }
    };
}

macro_rules! get_frame_mut {
    ($vm:ident) => {
        match $vm.frames.last_mut() {
            Some(frame) => frame,
            None => $vm.fatal_error(STACK_UNDERFLOW),
        }
    };
}

macro_rules! get_object {
    ($vm:ident, $ptr:expr, $type:pat) => {{
        let obj = $vm.get_object($ptr);
        match obj {
            $type => obj,
            _ => $vm.fatal_error(MEMORY_ERROR),
        }
    }};
}
macro_rules! get_object_mut {
    ($vm:ident, $ptr:expr, $type:pat) => {{
        let obj: *mut Object = $vm.get_object_mut($ptr);
        match unsafe { &mut *obj as &mut Object } {
            $type => unsafe { &mut *obj as &mut Object },
            _ => $vm.fatal_error(MEMORY_ERROR),
        }
    }};
}

pub mod op {
    #[repr(u8)]
    enum Op {
        Add,
        And,
        Assert,
        Call,
        CloseUpvalue,
        Closure,
        Constant,
        DefineGlobal,
        Divide,
        Equal,
        GetGlobal,
        GetLocal,
        GetUpvalue,
        Greater,
        GreaterEqual,
        Jump,
        JumpIfFalse,
        Loop,
        Multiply,
        Negate,
        Nil,
        Not,
        Or,
        Pop,
        Print,
        Return,
        SetGlobal,
        SetLocal,
        SetUpvalue,
        Subtract,
    }
    use Op::*;

    pub const ADD: u8 = Add as u8;
    pub const AND: u8 = And as u8;
    pub const ASSERT: u8 = Assert as u8;
    pub const CALL: u8 = Call as u8;
    pub const CLOSE_UPVALUE: u8 = CloseUpvalue as u8;
    pub const CLOSURE: u8 = Closure as u8;
    pub const CONSTANT: u8 = Constant as u8;
    pub const DEFINE_GLOBAL: u8 = DefineGlobal as u8;
    pub const DIVIDE: u8 = Divide as u8;
    pub const EQUAL: u8 = Equal as u8;
    pub const GET_GLOBAL: u8 = GetGlobal as u8;
    pub const GET_LOCAL: u8 = GetLocal as u8;
    pub const GET_UPVALUE: u8 = GetUpvalue as u8;
    pub const GREATER: u8 = Greater as u8;
    pub const GREATER_EQUAL: u8 = GreaterEqual as u8;
    pub const JUMP: u8 = Jump as u8;
    pub const JUMP_IF_FALSE: u8 = JumpIfFalse as u8;
    pub const LOOP: u8 = Loop as u8;
    pub const MULTIPLY: u8 = Multiply as u8;
    pub const NEGATE: u8 = Negate as u8;
    pub const NIL: u8 = Nil as u8;
    pub const NOT: u8 = Not as u8;
    pub const OR: u8 = Or as u8;
    pub const POP: u8 = Pop as u8;
    pub const PRINT: u8 = Print as u8;
    pub const RETURN: u8 = Return as u8;
    pub const SET_GLOBAL: u8 = SetGlobal as u8;
    pub const SET_LOCAL: u8 = SetLocal as u8;
    pub const SET_UPVALUE: u8 = SetUpvalue as u8;
    pub const SUBTRACT: u8 = Subtract as u8;
}

/// I just did this because Clippy told me to.
impl Default for VM {
    /// Create a new VM instance and set up its compiler
    /// which will produce the bytecode.
    ///
    /// Also fully initialize its memory, stack, and instruction
    /// pointer (which will point at the first instruction in
    /// the top-level-function that the script compiles to).
    fn default() -> Self {
        VM {
            stack: Vec::new(),
            globals: HashMap::new(),
            memory: Memory::default(),
            frames: Vec::new(),
            open_upvalues: Vec::new(),
            error_chain: LoxErrorChain::default(),
        }
    }
}

impl VM {
    /// Compile the script or line of code into bytecode, then
    /// execute the bytecode, all in the context of the VM that
    /// is set up with [new](#method.new).
    pub fn interpret(mut self, statement: &str) -> Result<(VM, Value), VM> {
        let mut scanner = Scanner::default();
        let mut parser = Parser::default();
        let compiler = Compiler::new(&mut scanner, &mut parser, &mut self);

        // Pass in the VM that calls the compiler so that the
        // compiler can swap itself out for a child compiler
        let function = match compiler.compile(statement) {
            Ok(func) => func,
            Err(mut error_chain) => {
                for error in error_chain.errors() {
                    self.error_chain.register(error);
                }
                return Err(self);
            }
        };

        self.stack.push(Value::Object(function));
        let function = self.memory.allocate(Object::Closure(Box::new(Closure {
            enclosing: None,
            upvalues: Vec::new(),
            function,
        })));

        self.call(function, 0, 0);
        match self.run() {
            Ok(value) => Ok((self, value)),
            Err(_) => Err(self),
        }
    }

    fn call(&mut self, mem_entry: MemoryEntry, argc: usize, slots: usize) {
        self.debug_trace_function(mem_entry);

        let function = self.get_closure_mut(mem_entry).function;
        let function = self.get_function(function);
        if function.arity != argc {
            let message = &format!("Expected {} arguments but got {}", function.arity, argc);
            self.runtime_error(message);
        }

        if self.frames.len() >= MAX_FRAMES {
            self.runtime_error(STACK_OVERFLOW);
        }

        self.frames.push(CallFrame {
            closure: mem_entry,
            ip: 0,
            slots,
        });
    }

    fn runtime_error(&mut self, message: &str) {
        let message = message.to_string();
        let line_number = self.line_number();

        self.error_chain.register(LoxError::RuntimeError {
            message,
            line: line_number,
        })
    }

    pub fn print_errors(&self) {
        self.error_chain.print_all();
    }

    pub fn run(&mut self) -> Result<Value, &LoxErrorChain> {
        loop {
            self.debug_trace_instruction();
            let op = self.read_byte();

            match op {
                op::ASSERT => {
                    let val = pop_stack!(self);

                    match val {
                        Value::Boolean(false) | Value::Nil => {
                            self.runtime_error(&format!("Value was {}", val));
                            self.fatal_error("Assertion Failed!!!");
                        }
                        _ => (),
                    }
                }
                op::CONSTANT => {
                    let constant = self.read_constant();
                    self.stack.push(constant);
                }
                op::DEFINE_GLOBAL => {
                    let name = self.read_name();
                    self.globals.insert(name, pop_stack!(self));
                }
                op::GET_GLOBAL => {
                    let name = &self.read_name();
                    let global = match self.globals.get(name).copied() {
                        Some(mem) => mem,
                        None => {
                            self.runtime_error(GLOBAL_ERROR);
                            return Err(&self.error_chain);
                        }
                    };

                    self.stack.push(global)
                }
                op::SET_GLOBAL => {
                    let val = pop_stack!(self);
                    let name = self.read_name();

                    self.globals.insert(name, val);
                    self.stack.push(val) // pancake
                }
                op::GET_LOCAL => {
                    let index = self.get_local();

                    // Increment the local by one so we have to reference the call frame
                    // one lower. The benefit is we can have a full 256 values on the
                    // stack as local variables.
                    self.stack.push(*get_stack!(self, index + 1))
                }
                op::SET_LOCAL => {
                    let index = self.get_local();
                    self.stack[index + 1] = *peek_stack!(self);
                }
                op::GET_UPVALUE => {
                    let upvalue = self.read_upvalue();
                    let value = self.get_upvalue(upvalue).value(self);
                    self.stack.push(value);
                }
                op::SET_UPVALUE => {
                    let value = pop_stack!(self);
                    let upvalue = self.read_upvalue();
                    self.set_upvalue(upvalue, value);
                }
                op::CLOSURE => {
                    let function = self.read_constant().as_pointer();
                    let upvalue_count = self.read_byte() as usize;

                    let mut upvalues = Vec::new();

                    for _ in 0..upvalue_count {
                        let is_local = self.read_byte() != 0;
                        let index = self.read_byte() as usize;

                        let upvalue = self.allocate_upvalue(is_local, index);
                        upvalues.push(upvalue);
                    }

                    let closure = Closure {
                        enclosing: Some(get_frame!(self).closure),
                        function,
                        upvalues,
                    };

                    let ptr = self.memory.allocate(Object::Closure(Box::new(closure)));

                    self.stack.push(Value::Object(ptr))
                }
                op::JUMP => {
                    let jump = self.read_jump();
                    get_frame_mut!(self).ip += jump;
                }
                op::JUMP_IF_FALSE => {
                    let condition = pop_stack!(self).as_boolean() as usize;
                    let condition = 1 - condition;

                    let jump = self.read_jump();
                    let jump = jump * condition;
                    get_frame_mut!(self).ip += jump;
                }
                op::LOOP => {
                    let jump = self.read_jump();
                    get_frame_mut!(self).ip -= jump;
                }
                op::POP => {
                    self.stack.pop();
                }
                op::PRINT => {
                    let val = pop_stack!(self);

                    match val {
                        Value::Nil => println!("nil"),
                        Value::Number(n) => println!("{}", n),
                        Value::Boolean(b) => println!("{}", b),
                        Value::Object(ptr) => println!("{}", self.show_object(ptr)),
                    }
                }
                op::NIL => self.stack.push(Value::Nil),
                op::NEGATE => {
                    let val = pop_stack!(self);

                    match val {
                        Value::Number(n) => self.stack.push(Value::Number(-n)),
                        _ => self.runtime_error("Cannot negate non-number."),
                    }
                }
                op::NOT => {
                    let val = pop_stack!(self);

                    let opposite = matches!(val, Value::Nil | Value::Boolean(false));
                    self.stack.push(Value::Boolean(opposite))
                }
                op::CALL => {
                    let argc = self.read_byte();
                    let frame = self.stack.len() - (argc as usize) - 1;
                    let closure = get_stack!(self, frame).as_pointer();
                    self.call(closure, argc as usize, frame);
                }
                op::RETURN => {
                    let value = pop_stack!(self);
                    let frame = pop_frame!(self);

                    if self.frames.is_empty() {
                        return Ok(value);
                    }
                    self.debug_trace_function(get_frame!(self).closure);

                    self.close_upvalues(frame.slots);
                    while self.stack.len() > frame.slots {
                        self.stack.pop();
                    }

                    self.stack.push(value);
                }
                op::CLOSE_UPVALUE => {
                    self.close_upvalues(self.stack.len() - 1);
                    self.stack.pop();
                }
                op::ADD => {
                    // Reverse order of arguments a and b to match lexical order
                    let b = pop_stack!(self);
                    let a = pop_stack!(self);

                    if self.is_string(&a) && self.is_string(&b) {
                        let a = a.as_pointer();
                        let b = b.as_pointer();

                        let string = self.concatenate(a, b);
                        self.stack.push(string)
                    } else if let (Some(a), Some(b)) = (a.as_number(), b.as_number()) {
                        self.stack.push(Value::Number(a + b))
                    } else {
                        self.runtime_error("Operands must be two numbers or two strings.")
                    }
                }
                op::SUBTRACT => {
                    // Popped in reverse order they were pushed, expecting a-b
                    let b = pop_stack!(self);
                    let a = pop_stack!(self);

                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        self.stack.push(Value::Number(a - b))
                    } else {
                        self.runtime_error("Cannot subtract non-numbers")
                    }
                }
                op::MULTIPLY => {
                    // Popped in reverse order they were pushed, expecting a-b
                    let b = pop_stack!(self);
                    let a = pop_stack!(self);

                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        self.stack.push(Value::Number(a * b))
                    } else {
                        self.runtime_error("Cannot multiply non-numbers")
                    }
                }
                op::DIVIDE => {
                    // Popped in reverse order they were pushed, expecting a-b
                    let b = pop_stack!(self);
                    let a = pop_stack!(self);

                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        self.stack.push(Value::Number(a / b))
                    } else {
                        self.runtime_error("Cannot divide non-numbers")
                    }
                }
                op::AND => {
                    let v1 = pop_stack!(self).as_boolean();
                    let v2 = pop_stack!(self).as_boolean();

                    self.stack.push(Value::Boolean(v1 && v2))
                }
                op::EQUAL => {
                    let b = pop_stack!(self);
                    let a = pop_stack!(self);

                    self.stack.push(Value::Boolean(a.strict_equals(&b)))
                }
                op::GREATER => {
                    let b = pop_stack!(self);
                    let a = pop_stack!(self);

                    if let (Some(a), Some(b)) = (a.as_number(), b.as_number()) {
                        self.stack.push(Value::Boolean(a > b))
                    } else {
                        self.runtime_error("Cannot compare two non-numbers.")
                    }
                }
                op::GREATER_EQUAL => {
                    let b = pop_stack!(self);
                    let a = pop_stack!(self);

                    if let (Some(a), Some(b)) = (a.as_number(), b.as_number()) {
                        self.stack.push(Value::Boolean(a >= b))
                    } else {
                        self.runtime_error("Cannot compare two non-numbers.")
                    }
                }
                op::OR => {
                    let v1 = pop_stack!(self).as_boolean();
                    let v2 = pop_stack!(self).as_boolean();

                    self.stack.push(Value::Boolean(v1 || v2))
                }
                30_u8..=u8::MAX => {
                    let msg = format!("Invalid bytecode {}", op);
                    self.fatal_error(&msg)
                }
            }
        }
    }

    pub fn current_closure_mut(&mut self) -> &mut Closure {
        let frame = get_frame!(self).closure;
        self.get_closure_mut(frame)
    }

    pub fn current_closure(&self) -> &Closure {
        let frame = get_frame!(self).closure;
        self.get_closure(frame)
    }

    pub fn current_function(&self) -> &Function {
        let function = self.current_closure().function;
        self.get_function(function)
    }

    pub fn ip(&mut self) -> usize {
        get_frame!(self).ip
    }

    pub fn line_number(&self) -> usize {
        let frame = get_frame!(self).ip - 1;
        *self.current_function().chunk.lines.get(frame).unwrap()
    }

    fn byte(&mut self, ip: usize) -> u8 {
        match self.current_function().chunk.code.get(ip) {
            Some(byte) => *byte,
            None => {
                self.fatal_error(MEMORY_ERROR);
            }
        }
    }

    pub fn peek_byte(&mut self) -> u8 {
        self.byte(get_frame!(self).ip)
    }

    pub fn read_byte(&mut self) -> u8 {
        let ip = self.ip();
        get_frame_mut!(self).ip += 1;
        self.byte(ip)
    }

    pub fn read_constant(&mut self) -> Value {
        let index = self.read_byte() as usize;
        let constant = self.current_function().chunk.constants.get(index);
        match constant {
            Some(val) => *val,
            None => self.fatal_error(CONSTANT_ERROR),
        }
    }

    pub fn read_jump(&mut self) -> usize {
        let high = self.read_byte() as usize;
        let low = self.read_byte() as usize;

        (high << 8) | low
    }

    fn read_upvalue(&mut self) -> MemoryEntry {
        let index = self.read_byte();
        match self.current_closure().upvalues.get(index as usize) {
            None => self.fatal_error(STACK_UNDERFLOW),
            Some(mem) => *mem,
        }
    }

    fn fatal_error(&self, msg: &str) -> ! {
        panic!("{}\nErrors: {}", msg, self.error_chain.show_all())
    }

    pub fn get_object(&self, ptr: MemoryEntry) -> &Object {
        if self.memory.is_valid(ptr) {
            return self.memory.retrieve(&ptr).unwrap();
        }
        self.fatal_error(MEMORY_ERROR)
    }

    pub fn get_object_mut(&mut self, ptr: MemoryEntry) -> &mut Object {
        if self.memory.is_valid(ptr) {
            return self.memory.retrieve_mut(&ptr).unwrap();
        }
        self.fatal_error(MEMORY_ERROR)
    }

    pub(crate) fn show_object(&mut self, ptr: MemoryEntry) -> String {
        let function = match self.get_object(ptr) {
            Object::Closure(c) => c.function,
            obj => return format!("{}", obj),
        };
        self.show_object(function)
    }

    pub fn get_string(&self, ptr: MemoryEntry) -> &String {
        get_object!(self, ptr, Object::String(_)).as_string()
    }

    pub fn get_function_mut(&mut self, ptr: MemoryEntry) -> &mut Function {
        get_object_mut!(self, ptr, Object::Function(_)).as_function_mut()
    }

    pub fn get_function(&self, ptr: MemoryEntry) -> &Function {
        get_object!(self, ptr, Object::Function(_)).as_function()
    }

    pub fn get_closure_mut(&mut self, ptr: MemoryEntry) -> &mut Closure {
        get_object_mut!(self, ptr, Object::Closure(_)).as_closure_mut()
    }

    pub fn get_closure(&self, ptr: MemoryEntry) -> &Closure {
        get_object!(self, ptr, Object::Closure(_)).as_closure()
    }

    pub(crate) fn get_upvalue(&self, ptr: MemoryEntry) -> &Upvalue {
        get_object!(self, ptr, Object::Upvalue(_)).as_upvalue()
    }

    fn get_upvalue_mut(&mut self, ptr: MemoryEntry) -> &mut Upvalue {
        get_object_mut!(self, ptr, Object::Upvalue(_)).as_upvalue_mut()
    }

    pub fn is_string(&self, value: &Value) -> bool {
        match value {
            Value::Object(ptr) => {
                matches!(self.get_object(*ptr), Object::String(_))
            }
            _ => false,
        }
    }

    fn set_upvalue(&mut self, mut upvalue: MemoryEntry, value: Value) {
        while !self.get_upvalue(upvalue).is_root() {
            upvalue = self.get_upvalue(upvalue).get_parent();
        }

        if self.get_upvalue(upvalue).is_local() {
            let local = self.get_upvalue(upvalue).get_local();
            self.stack[local] = value;
            return;
        }

        self.get_upvalue_mut(upvalue).close(value);
    }

    pub fn concatenate(&mut self, v1: MemoryEntry, v2: MemoryEntry) -> Value {
        let mut result = self.get_object(v1).as_string().clone();
        result.push_str(self.get_object(v2).as_string());

        Value::Object(self.memory.allocate(Object::String(Box::new(result))))
    }

    fn get_local(&mut self) -> usize {
        let base = get_frame!(self).slots;
        let offset = self.read_byte() as usize;
        base + offset
    }

    fn allocate_upvalue(&mut self, is_local: bool, index: usize) -> MemoryEntry {
        let upvalue = if is_local {
            let base = get_frame!(self).slots;
            // Base points to the function of the closure that was called. The upvalue
            // will never be that function because if it was it would be alocal in the
            // calling scope or a global. What we mean by `index` here is the index of
            // the local variable, which we dereference as `index + 1`, if you look
            // at op::GET_LOCAL
            Upvalue::from_local(base + index + 1)
        } else {
            let upvalues = &self.current_closure().upvalues;
            let upvalue = *upvalues.get(index).unwrap();
            Upvalue::from_enclosing(upvalue)
        };

        let ptr = self.memory.allocate(Object::Upvalue(Box::new(upvalue)));

        // It's possible to create an upvalue that points to a parent upvalue that
        // has already gone out of scope. This happens when a closure is created
        // by another closure, whose upvalues are out of scope. For example:
        //
        // ```lox
        // var h;
        // {
        //     var x = 1;
        //     fun f() {
        //         // Closure will be created when `f` is called.
        //         fun g() {
        //             return x;
        //         }
        //         return g;
        //     }
        //     h = f;
        // }
        // h();
        // ```
        if upvalue.is_closed(self) {
            return ptr;
        }

        // todo: is there a more efficient way to do this than copying the whole
        // array without causing borrow checker errors?
        self.open_upvalues.push(ptr);
        let mut ou = self.open_upvalues.clone();
        ou.sort_by(|u, v| {
            let u = self.get_upvalue(*u).stack_index(self);
            let v = self.get_upvalue(*v).stack_index(self);

            u.cmp(&v)
        });

        std::mem::swap(&mut self.open_upvalues, &mut ou);

        ptr
    }

    fn read_name(&mut self) -> String {
        let name = self.read_constant().as_pointer();
        return self.get_object(name).as_string().clone();
    }

    fn close_upvalues(&mut self, stack_top: usize) {
        if self.open_upvalues.is_empty() {
            return;
        }

        loop {
            // This loop could potentially close all open upvalues, for example if
            // it is called after the only or last closure's variables go out of
            // scope, or the next closure hasn't been created yet.
            if self.open_upvalues.is_empty() {
                break;
            }

            let upvalue_ptr = *self.open_upvalues.last().unwrap();
            let upvalue = *self.get_upvalue(upvalue_ptr);
            if !upvalue.is_after(stack_top, self) {
                break;
            }

            let value = self.get_upvalue(upvalue_ptr).value(self);
            self.get_upvalue_mut(upvalue_ptr).close(value);

            self.open_upvalues.pop();
        }
    }
}

impl Display for LoxErrorChain {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err_type = match self {
            LoxError::ScanError { .. } => "ScanError",
            LoxError::ParseError { .. } => "ParseError",
            LoxError::RuntimeError { .. } => "RuntimeError",
        };

        match self {
            LoxError::RuntimeError { message, line }
            | LoxError::ParseError { message, line }
            | LoxError::ScanError { message, line } => {
                write!(f, "[line={}] {}: {}", line, err_type, message)
            }
        }
    }
}

impl Default for LoxErrorChain {
    fn default() -> LoxErrorChain {
        LoxErrorChain { errors: Vec::new() }
    }
}

impl LoxErrorChain {
    pub fn register(&mut self, error: LoxError) {
        self.errors.push(error)
    }

    pub(crate) fn had_error(&self) -> bool {
        !self.errors.is_empty()
    }

    pub(crate) fn show_all(&self) -> String {
        self.errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<String>>()
            .join("\n")
    }

    pub(crate) fn print_all(&self) {
        println!("{}", self.show_all())
    }

    pub fn errors(&mut self) -> Vec<LoxError> {
        std::mem::take(&mut self.errors)
    }
}

impl Error for LoxErrorChain {}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test_program_assertion {
        ($test_name:ident, $text:literal) => {
            #[test]
            #[should_panic]
            fn $test_name() {
                unsafe {
                    // Debug print in case the test fails, makes debugging easier
                    crate::debug::DEBUG_PRINT_CODE = true;
                    crate::debug::DEBUG_TRACE_EXECUTION = true;
                }
                match VM::default().interpret($text) {
                    Err(vm) => println!("!!! Error in compilation: {}", vm.error_chain),
                    _ => (),
                }
            }
        };
    }

    macro_rules! test_program_failure {
        ($test_name:ident, $text:literal) => {
            #[test]
            fn $test_name() {
                unsafe {
                    // Debug print in case the test fails, makes debugging easier
                    crate::debug::DEBUG_PRINT_CODE = true;
                    crate::debug::DEBUG_TRACE_EXECUTION = true;
                }
                // todo: assert on the kind of failure
                match VM::default().interpret($text) {
                    Err(_) => (),
                    _ => panic!("Program did not cause compilation or runtime error."),
                }
            }
        };
    }

    macro_rules! test_program {
        ($test_name:ident, $text:literal) => {
            #[test]
            fn $test_name() {
                unsafe {
                    // Debug print in case the test fails, makes debugging easier
                    crate::debug::DEBUG_PRINT_CODE = true;
                    crate::debug::DEBUG_TRACE_EXECUTION = true;
                }

                println!("Interpreting program:\n{}", $text);
                match VM::default().interpret($text) {
                    Ok(_) => (),
                    Err(vm) => {
                        panic!("!!! Error interpreting statement: {}", vm.error_chain)
                    }
                }
            }
        };
    }

    test_program! { hello_world_function, "print \"Hello, world!\";"}

    test_program! { assert_true, "assert true;" }

    test_program! {
        compare_two_numbers,
        "assert 1 == 1;"
    }

    test_program! {
        add_two_numbers,
        "assert 1 + 1 == 2;"
    }

    test_program! {
        greater_than_numbers,
        "
        assert 2 > 1;
        assert ! (1 > 1);
        assert ! (0 > 1);
        "
    }

    test_program! {
        greater_than_equal_numbers,
        "
        assert 2 >= 1;
        assert 1 >= 1;
        assert ! (0 > 1);
        "
    }

    test_program! {
        less_than_numbers,
        "
        assert ! (2 < 1);
        assert ! (1 < 1);
        assert 0 < 1;
        "
    }

    test_program! {
        less_than_equal_numbers,
        "
        assert ! (2 <= 1);
        assert 1 <= 1;
        assert 0 <= 1;
        "
    }

    test_program! {
        multiply_and_divide,
        "
        assert 2 * 2 == 4;
        assert 10 / 5 == 2;
        "
    }

    test_program! {
        define_and_reference_global_variable,
        "
        var x = 10;
        assert x == 10;
        "
    }

    test_program! {
        local_variables_block_scoping,
        "
        var x = 10;
        {
            var x = 20;
            assert x == 20;
        }
        assert x == 10;
        {
            var x = 30;
            assert x == 30;
        }
        "
    }

    test_program! {
        local_variables_get_and_set,
        "
        var x = 10;
        {
            var x;
            assert x == nil;

            x = 20;
            assert x == 20;
        }
        assert x == 10;
        "
    }

    test_program! {
        set_global_variable,
        "
        var x = 10;
        assert x == 10;

        x = 20;
        assert x == 20;
        "
    }

    test_program! {
        chained_assignment,
        "
        var x = 10;
        var y = 20;

        assert 30 == (x = y = 30);
        assert x == 30;
        assert y == 30;

        {
            var x = 40;
            var y = 50;

            assert 60 == (x = y = 60);
            assert x == 60;
            assert y == 60;
        }

        assert x == 30;
        assert y == 30;
        "
    }

    test_program! {
        chained_assignment_multi,
        "
        {
            var x = 10;
            var y = 20;
            var z = 30;

            assert 30 == (x = y = z);
        }

        {
            var t = 0;
            assert t == 0;
        }
        "
    }

    test_program! {
        concatenate_strings,
        // Actually tests that we intern the string by re-constructing
        "
        var x = \"Hello,\";
        var y = \"world!\";

        assert \"Hello, world!\" == x + \" \" + y;
        "
    }

    test_program! {
        if_statement_no_else,
        "
        var x = 1;
        if (true) x = 2;

        assert x == 2;
        "
    }

    test_program! {
        if_statement_with_statement_after,
        "
        var x = 1;
        if (false) assert false;

        assert x == 1;"
    }

    test_program! {
        if_statement,
        "
        var x = 1;
        if (false) assert false;
        else x = 2;

        assert x == 2;

        if (true) x = 3;
        else assert false;

        assert x == 3;
        "
    }

    test_program! {
        while_statement,
        "
        var x = 1;
        while (x < 10) x = x + 1;

        assert x == 10;
        "
    }

    test_program! {
        for_statement,
        "
        var y = 0;
        var x = 10;
        for (var x = 0; x < 10; x = x + 1) {
            assert x < 10;
            assert x == y;
            y = y + 1;
        }
        assert y == 10;
        assert x == 10;
        "
    }

    test_program! {
        define_function_global,
        "
        fun f(a, b) {
            assert true;
        }
        "
    }

    test_program! {
        define_function,
        "
        var f = 10;
        {
            fun f(a, b) {
                assert true;
            }
        }

        assert f == 10;
        "
    }

    test_program! {
        call_function,
        "
        fun f(x, y) {
            return x + y;
        }

        assert 3 == f(1, 2);
        "
    }

    test_program! {
        return_function_as_value,
        "
        fun f() {
            fun g() {
                return 1;
            }
            return g;
        }

        assert f()() == 1;
        "
    }

    test_program! {
        close_value,
        "
        var x = 1;
        fun f() {
            return x;
        }

        assert f() == 1;

        x = 2;

        assert f() == 2;
        "
    }

    test_program! {
        close_local_variables,
        "
        var g;
        {
            var x = 1;
            fun f() {
                return x;
            }
            g = f;
        }

        assert g() == 1;
        "
    }

    test_program! {
        multiple_closed_variables,
        "
        {
            var x = 1;
            var y = 2;
            fun f() {
                return x + y;
            }
        
            assert 3 == f();
        }
        "
    }

    test_program! {
        counter,
        "
        fun counter() {
            var x = 0;
            fun c() {
                x = x + 1;
                return x;
            }
            return c;
        }

        var t = counter();
        var r = counter();

        assert 1 == t();
        assert 2 == t();
        assert 1 == r();
        "
    }

    test_program_failure! {
        reference_unset_global_variable_exits_early,
        "
        {
            var y = x + 1;
        }
        assert false;
        "
    }

    test_program_assertion! {
        assert_failure_to_prove_function_called,
        "
        fun f() {
            return;
        }

        f();

        assert false;
        "
    }

    test_program_assertion! {
        assert_false,
        "assert false;"
    }
}
