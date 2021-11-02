use crate::vm::op::*;

pub trait DisassembleInstruction {
    fn bytecode_name(&self) -> String;
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
            22_u8..=u8::MAX => panic!("Invalid Opcode."),
        }
    }
}
