use crate::compiler::Function;
use crate::value::Value;
use crate::vm::{op::*, VM};

pub static mut DEBUG_GRAPH_CODE: bool = false;
pub static mut DEBUG_PRINT_CODE: bool = true;
pub static mut DEBUG_TRACE_EXECUTION: bool = false;

pub trait Disassembler {
    fn disassemble_chunk(&self);
}

pub trait GraphAssembly {
    fn graph_output_chunk(&self);
}

pub trait DebugTrace {
    fn debug_trace_instruction(&self);
    fn debug_trace_function(&self);
}

trait DisassembleInstruction {
    fn bytecode_name(&self) -> String;
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
            let op = *self.chunk.code.get(i).unwrap();

            let action = match op {
                ADD => Self::print_instruction,
                AND => Self::print_instruction,
                ASSERT => Self::print_instruction,
                CONSTANT => Self::print_constant,
                DEFINE_GLOBAL => Self::print_constant,
                DIVIDE => Self::print_instruction,
                EQUAL => Self::print_instruction,
                GET_GLOBAL => Self::print_constant,
                GET_LOCAL => Self::print_local,
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
                SUBTRACT => Self::print_instruction,
                25_u8..=u8::MAX => panic!("Invalid opcode."),
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
            let op = *self.chunk.code.get(i).unwrap();

            let action = match op {
                ADD => Self::graph_instruction,
                AND => Self::graph_instruction,
                ASSERT => Self::graph_instruction,
                CONSTANT => Self::graph_constant,
                DEFINE_GLOBAL => Self::graph_constant,
                DIVIDE => Self::graph_instruction,
                EQUAL => Self::graph_instruction,
                GET_GLOBAL => Self::graph_constant,
                GET_LOCAL => Self::graph_local,
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
                SUBTRACT => Self::graph_instruction,
                25_u8..=u8::MAX => panic!("Invalid opcode."),
            };

            i = action(self, i, op);
        }

        eprintln!("}}")
    }
}

impl DebugTrace for VM {
    fn debug_trace_function(&self) {
        unsafe {
            if !DEBUG_TRACE_EXECUTION {
                return;
            }
        }

        eprintln!(
            "----- execute::{} -----",
            if !self.current_closure().name.is_empty() {
                &self.current_closure().name
            } else {
                "<script>"
            }
        );
    }

    fn debug_trace_instruction(&self) {
        unsafe {
            if !DEBUG_TRACE_EXECUTION {
                return;
            }
        }

        let ip = self.ip();
        let line_part = self.get_line_part(ip);

        let op = *self.current_closure().chunk.code.get(ip).unwrap();
        let op = op.bytecode_name();

        let show_val = |v: &Value| -> String {
            match v {
                Value::Object(ptr) => format!("{}", self.memory.retrieve(ptr)),
                _ => format!("{}", v),
            }
        };

        let stack = self
            .stack
            .iter()
            .map(show_val)
            .collect::<Vec<String>>()
            .join(", ");
        eprintln!("{:04} {}{:16} {}", ip, line_part, op, stack);
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

    fn graph_local(&self, i: usize, op: u8) -> usize {
        let c = *self.chunk.code.get(i + 1).unwrap();
        let c = self.chunk.constants.get(c as usize).unwrap();
        let next = self.chunk.code.get(i + 2).unwrap();

        eprintln!("\"{}: {:?}\" -> \"{}: {}\";", i, op, i + 1, c);
        eprintln!("\"{}: {:?}\" -> \"{}: {:?}\";", i, op, i + 2, next);

        i + 2
    }

    fn graph_constant(&self, i: usize, op: u8) -> usize {
        // todo: graph the constant itself, not the pointer
        let c = self.chunk.code.get(i + 1).unwrap();
        let next = self.chunk.code.get(i + 2).unwrap();

        let c = self.chunk.constants.get(*c as usize).unwrap();

        eprintln!("\"{}: {:?}\" -> \"{}: {}\";", i, op, i + 1, c);
        eprintln!("\"{}: {:?}\" -> \"{}: {:?}\";", i, op, i + 2, next);

        i + 2
    }

    fn graph_instruction(&self, i: usize, op: u8) -> usize {
        let next = self.chunk.code.get(i + 1).unwrap();
        eprintln!("\"{}: {:?}\" -> \"{}: {:?}\";", i, op, i + 1, next);

        i + 1
    }

    fn print_jump(&self, i: usize, op: String) -> usize {
        let byte1 = *self.chunk.code.get(i + 1).unwrap();
        let byte2 = *self.chunk.code.get(i + 2).unwrap();
        let jump = Self::read_jump(byte1, byte2);
        let line_part = self.get_line_part(i);

        eprintln!("{:04} {}{:16} -> {:6}", i, line_part, op, jump);

        i + 3
    }

    fn print_local(&self, i: usize, op: String) -> usize {
        // todo: graph the values instead of the pointer
        let val = self.chunk.code.get(i + 1).unwrap();
        let line_part = self.get_line_part(i);

        eprintln!("{:04} {}{:16}{:10}", i, line_part, op, val);
        i + 2
    }

    fn print_constant(&self, i: usize, op: String) -> usize {
        // todo: graph the values instead of the pointer
        let val = self.chunk.code.get(i + 1).unwrap();
        let line_part = self.get_line_part(i);

        eprintln!(
            "{:04} {}{:16}{:10} '{}'",
            i,
            line_part,
            op,
            val,
            self.chunk.constants.get(*val as usize).unwrap()
        );
        i + 2
    }

    fn print_instruction(&self, i: usize, op: String) -> usize {
        let line_part = self.get_line_part(i);
        eprintln!("{:04} {}{}", i, line_part, op);
        i + 1
    }

    fn get_line_part(&self, i: usize) -> String {
        if i == 0 {
            return format!("{:<4}", "|");
        }

        let line = *self.chunk.lines.get(i - 1).unwrap();
        let this_line = *self.chunk.lines.get(i).unwrap();
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
    fn get_line_part(&self, i: usize) -> String {
        if i == 0 {
            return format!("{:<4}", "|");
        }

        let line = *self.current_closure().chunk.lines.get(i - 1).unwrap();
        let this_line = *self.current_closure().chunk.lines.get(i).unwrap();
        if this_line > line {
            format!("{:<4}", this_line)
        } else {
            format!("{:<4}", "|")
        }
    }
}

impl DisassembleInstruction for u8 {
    fn bytecode_name(&self) -> String {
        match *self {
            ADD => String::from("OP_ADD"),
            AND => String::from("OP_AND"),
            ASSERT => String::from("OP_ASSERT"),
            CONSTANT => String::from("OP_CONSTANT"),
            DEFINE_GLOBAL => String::from("OP_DEFINE_GLOBAL"),
            DIVIDE => String::from("OP_DIVIDE"),
            EQUAL => String::from("OP_EQUAL"),
            GET_GLOBAL => String::from("OP_GET_GLOBAL"),
            GET_LOCAL => String::from("OP_GET_LOCAL"),
            GREATER => String::from("OP_GREATER"),
            GREATER_EQUAL => String::from("OP_GREATER_EQUAL"),
            JUMP => String::from("OP_JUMP"),
            JUMP_IF_FALSE => String::from("OP_JUMP_IF_FALSE"),
            LOOP => String::from("OP_LOOP"),
            MULTIPLY => String::from("OP_MULTIPLY"),
            NEGATE => String::from("OP_NEGATE"),
            NIL => String::from("OP_NIL"),
            NOT => String::from("OP_NOT"),
            OR => String::from("OP_OR"),
            POP => String::from("OP_POP"),
            PRINT => String::from("OP_PRINT"),
            RETURN => String::from("OP_RETURN"),
            SET_GLOBAL => String::from("OP_SET_GLOBAL"),
            SET_LOCAL => String::from("OP_SET_LOCAL"),
            SUBTRACT => String::from("OP_SUBTRACT"),
            25_u8..=u8::MAX => panic!("Invalid Opcode."),
        }
    }
}
