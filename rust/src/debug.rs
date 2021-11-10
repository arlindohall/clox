use crate::compiler::Function;
use crate::object::MemoryEntry;
use crate::value::Value;
use crate::vm::{op::*, VM};

pub static mut DEBUG_GRAPH_CODE: bool = false;
pub static mut DEBUG_PRINT_CODE: bool = true;
pub static mut DEBUG_TRACE_EXECUTION: bool = true;

pub trait Disassembler {
    fn disassemble_chunk(&self);
}

pub trait GraphAssembly {
    fn graph_output_chunk(&self);
}

pub trait DebugTrace {
    fn debug_trace_instruction(&mut self);
    fn debug_trace_function(&mut self, ptr: MemoryEntry);
}

trait DisassembleInstruction {
    fn bytecode_name(&self) -> &str;
}

impl Disassembler for Function {
    fn disassemble_chunk(&self) {
        unsafe {
            if !DEBUG_PRINT_CODE {
                return;
            }
        }

        eprintln!(
            "===== chunk {} =====",
            if !self.name.is_empty() {
                &self.name
            } else {
                "<script>"
            }
        );
        let mut i = 0;

        while i < self.chunk.code.len() {
            let op = self.chunk.code[i];

            let action = match op {
                ADD => Self::print_instruction,
                AND => Self::print_instruction,
                ASSERT => Self::print_instruction,
                CALL => Self::print_local,
                CLOSE_UPVALUE => Self::print_instruction,
                CLOSURE => Self::print_closure,
                CONSTANT => Self::print_constant,
                DEFINE_GLOBAL => Self::print_constant,
                DIVIDE => Self::print_instruction,
                EQUAL => Self::print_instruction,
                GET_GLOBAL => Self::print_constant,
                GET_LOCAL => Self::print_local,
                GET_UPVALUE => Self::print_local,
                GREATER => Self::print_instruction,
                GREATER_EQUAL => Self::print_instruction,
                JUMP => Self::print_jump,
                JUMP_IF_FALSE => Self::print_jump,
                LOOP => Self::print_jump,
                MULTIPLY => Self::print_instruction,
                NEGATE => Self::print_instruction,
                NIL => Self::print_instruction,
                NOT => Self::print_instruction,
                OR => Self::print_instruction,
                POP => Self::print_instruction,
                PRINT => Self::print_instruction,
                RETURN => Self::print_instruction,
                SET_GLOBAL => Self::print_constant,
                SET_LOCAL => Self::print_local,
                SET_UPVALUE => Self::print_local,
                SUBTRACT => Self::print_instruction,
                30_u8..=u8::MAX => panic!("Invalid opcode."),
            };

            i = action(self, i, op.bytecode_name());
        }
    }
}

impl GraphAssembly for Function {
    fn graph_output_chunk(&self) {
        unsafe {
            if !DEBUG_GRAPH_CODE {
                return;
            }
        }

        eprintln!("digraph chunk {{");
        let mut i = 0;

        while i < self.chunk.code.len() - 1 {
            let op = self.chunk.code[i];

            let action = match op {
                ADD => Self::graph_instruction,
                AND => Self::graph_instruction,
                ASSERT => Self::graph_instruction,
                CALL => Self::graph_local,
                CLOSE_UPVALUE => Self::graph_instruction,
                CLOSURE => Self::graph_closure,
                CONSTANT => Self::graph_constant,
                DEFINE_GLOBAL => Self::graph_constant,
                DIVIDE => Self::graph_instruction,
                EQUAL => Self::graph_instruction,
                GET_GLOBAL => Self::graph_constant,
                GET_LOCAL => Self::graph_local,
                GET_UPVALUE => Self::graph_local,
                GREATER => Self::graph_instruction,
                GREATER_EQUAL => Self::graph_instruction,
                JUMP => Self::graph_jump,
                JUMP_IF_FALSE => Self::graph_jump,
                LOOP => Self::graph_jump,
                MULTIPLY => Self::graph_instruction,
                NEGATE => Self::graph_instruction,
                NIL => Self::graph_instruction,
                NOT => Self::graph_instruction,
                OR => Self::graph_instruction,
                POP => Self::graph_instruction,
                PRINT => Self::graph_instruction,
                RETURN => Self::graph_instruction,
                SET_GLOBAL => Self::graph_constant,
                SET_LOCAL => Self::graph_local,
                SET_UPVALUE => Self::graph_local,
                SUBTRACT => Self::graph_instruction,
                30_u8..=u8::MAX => panic!("Invalid opcode."),
            };

            i = action(self, i, op);
        }

        eprintln!("}}")
    }
}

impl DebugTrace for VM {
    fn debug_trace_instruction(&mut self) {
        unsafe {
            if !DEBUG_TRACE_EXECUTION {
                return;
            }
        }

        let ip = self.ip();
        let line_part = self.get_line_part(ip);

        let op = self.current_function().chunk.code[ip];
        let op = op.bytecode_name();

        // todo: this is expensive and happens on every instruction, is there a way to avoid?
        let stack = self.stack.clone();

        let show_val = |v: &Value| -> String {
            match v {
                Value::Object(ptr) => self.show_object(*ptr),
                _ => format!("{}", v),
            }
        };

        let stack = stack
            .iter()
            .map(show_val)
            .collect::<Vec<String>>()
            .join(", ");
        eprintln!("{:04} {}{:16} {}", ip, line_part, op, stack);
    }

    fn debug_trace_function(&mut self, ptr: MemoryEntry) {
        unsafe {
            if !DEBUG_TRACE_EXECUTION {
                return;
            }
        }
        let function = self.get_closure_mut(ptr).function;
        let function = self.get_function(function);

        eprintln!(
            "----- function =={}== -----",
            if !function.name.is_empty() {
                &function.name
            } else {
                "<script>"
            }
        );
    }
}

impl Function {
    fn graph_jump(&self, i: usize, op: u8) -> usize {
        todo!(
            "graph jump instruction with arrow to jump point {} {}",
            i,
            op
        )
    }

    fn graph_closure(&self, i: usize, op: u8) -> usize {
        todo!("graph closure instruction with arrow to chunk {} {}", i, op)
    }

    fn graph_local(&self, i: usize, op: u8) -> usize {
        let c = self.chunk.code[i + 1];
        let next = self.chunk.code[i + 2];

        eprintln!("\"{}: {:?}\" -> \"{}: {}\";", i, op, i + 1, c);
        eprintln!("\"{}: {:?}\" -> \"{}: {:?}\";", i, op, i + 2, next);

        i + 2
    }

    fn graph_constant(&self, i: usize, op: u8) -> usize {
        // todo: graph the constant itself, not the pointer
        let c = self.chunk.code[i + 1];
        let next = self.chunk.code[i + 2];

        let c = self.chunk.constants[c as usize];

        eprintln!("\"{}: {}\" -> \"{}: {}\";", i, op, i + 1, c);
        eprintln!("\"{}: {}\" -> \"{}: {}\";", i, op, i + 2, next);

        i + 2
    }

    fn graph_instruction(&self, i: usize, op: u8) -> usize {
        let next = self.chunk.code[i + 1];
        eprintln!("\"{}: {:?}\" -> \"{}: {:?}\";", i, op, i + 1, next);

        i + 1
    }

    fn print_jump(&self, i: usize, op: &str) -> usize {
        let byte1 = self.chunk.code[i + 1];
        let byte2 = self.chunk.code[i + 2];
        let jump = Self::read_jump(byte1, byte2);
        let line_part = self.get_line_part(i);

        eprintln!("{:04} {}{:16} -> {:6}", i, line_part, op, jump);

        i + 3
    }

    fn print_closure(&self, mut i: usize, op: &str) -> usize {
        let line_part = self.get_line_part(i);
        let function = self.chunk.code[i + 1];
        let upval_count = self.chunk.code[i + 2] as usize;
        i += 3;

        eprintln!(
            "{:04} {}{:16}{:10}, {}:",
            i, line_part, op, function, upval_count
        );

        for upv in 0..upval_count {
            eprintln!(
                "           UPVALUE(is_local={}, index={}) ",
                self.chunk.code[i + (2 * upv)] != 0,
                self.chunk.code[i + (2 * upv) + 1]
            )
        }

        i + (upval_count * 2)
    }

    fn print_local(&self, i: usize, op: &str) -> usize {
        // todo: graph the values instead of the pointer
        let val = self.chunk.code[i + 1];
        let line_part = self.get_line_part(i);

        eprintln!("{:04} {}{:16}{:10}", i, line_part, op, val);
        i + 2
    }

    fn print_constant(&self, i: usize, op: &str) -> usize {
        // todo: graph the values instead of the pointer
        let val = self.chunk.code[i + 1];
        let line_part = self.get_line_part(i);

        eprintln!(
            "{:04} {}{:16}{:10} '{}'",
            i,
            line_part,
            op,
            val,
            self.chunk.constants[val as usize]
        );
        i + 2
    }

    fn print_instruction(&self, i: usize, op: &str) -> usize {
        let line_part = self.get_line_part(i);
        eprintln!("{:04} {}{}", i, line_part, op);
        i + 1
    }

    fn get_line_part(&self, i: usize) -> String {
        if i == 0 {
            return format!("{:<4}", "|");
        }

        let line = self.chunk.lines[i - 1];
        let this_line = self.chunk.lines[i];
        if this_line > line {
            format!("{:<4}", this_line)
        } else {
            format!("{:<4}", "|")
        }
    }

    fn read_jump(byte1: u8, byte2: u8) -> usize {
        let high = byte1 as usize;
        let low = byte2 as usize;

        (high << 8) | low
    }
}

impl VM {
    fn get_line_part(&mut self, i: usize) -> String {
        if i == 0 {
            return format!("{:<4}", "|");
        }

        let line = self.current_function().chunk.lines[i - 1];
        let this_line = self.current_function().chunk.lines[i];
        if this_line > line {
            format!("{:<4}", this_line)
        } else {
            format!("{:<4}", "|")
        }
    }
}

impl DisassembleInstruction for u8 {
    fn bytecode_name(&self) -> &str {
        match *self {
            ADD => "OP_ADD",
            AND => "OP_AND",
            ASSERT => "OP_ASSERT",
            CALL => "OP_CALL",
            CLOSURE => "OP_CLOSURE",
            CLOSE_UPVALUE => "OP_CLOSE_UPVALUE",
            CONSTANT => "OP_CONSTANT",
            DEFINE_GLOBAL => "OP_DEFINE_GLOBAL",
            DIVIDE => "OP_DIVIDE",
            EQUAL => "OP_EQUAL",
            GET_GLOBAL => "OP_GET_GLOBAL",
            GET_LOCAL => "OP_GET_LOCAL",
            GET_UPVALUE => "OP_GET_UPVALUE",
            GREATER => "OP_GREATER",
            GREATER_EQUAL => "OP_GREATER_EQUAL",
            JUMP => "OP_JUMP",
            JUMP_IF_FALSE => "OP_JUMP_IF_FALSE",
            LOOP => "OP_LOOP",
            MULTIPLY => "OP_MULTIPLY",
            NEGATE => "OP_NEGATE",
            NIL => "OP_NIL",
            NOT => "OP_NOT",
            OR => "OP_OR",
            POP => "OP_POP",
            PRINT => "OP_PRINT",
            RETURN => "OP_RETURN",
            SET_GLOBAL => "OP_SET_GLOBAL",
            SET_LOCAL => "OP_SET_LOCAL",
            SET_UPVALUE => "OP_SET_UPVALUE",
            SUBTRACT => "OP_SUBTRACT",
            30_u8..=u8::MAX => panic!("Invalid Opcode."),
        }
    }
}
