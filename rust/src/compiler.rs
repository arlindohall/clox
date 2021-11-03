use crate::debug::Disassembler;
use crate::debug::GraphAssembly;
use crate::object::MemoryEntry;
use crate::object::Object;
use crate::scanner::Scanner;
use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::scanner::TokenType::*;
use crate::value::Value;
use crate::vm::op;
use crate::vm::LoxError::ParseError;
use crate::vm::LoxErrorChain;
use crate::vm::VM;

const UNINITIALIZED: isize = -1;

/// Compiler used for a single function or script.
///
/// The compiler needs a lifetime because it stores a reference to
/// the source string in `scanner.source`. The source string, though
/// isn't just the full program text passed in from the script interpreter,
/// it could also be the most recent line from the repl. In the latter
/// case, the source will be dropped on the next repl loop iteration.
#[derive(Debug)]
pub struct Compiler<'a> {
    vm: &'a mut VM,

    ancestors: Vec<Compiler<'a>>,

    scanner: Scanner,
    parser: Parser,

    function: MemoryEntry,
    locals: Vec<Local>,

    scope_depth: isize,

    error_chain: LoxErrorChain,
}

#[derive(Debug)]
pub struct Local {
    name: Token,
    depth: isize,
    is_captured: bool,
}

/// The parser that does all the work creating the bytecode.
#[derive(Debug)]
struct Parser {
    current: Token,
    previous: Token,

    panic_mode: bool,
}

/// A static function.
///
/// This is the result of the [Compiler] method `compile`
/// because the top-level script is basically an immediately
/// invoked function.
///
/// The function holds onto compiled constant values and
/// anything it needs to be invoked as a closure will be
/// stored in a closure structure.
#[derive(Debug)]
pub struct Function {
    pub(crate) name: String,
    pub(crate) chunk: Chunk,
    pub(crate) arity: usize,
}

#[derive(Debug)]
pub(crate) struct Chunk {
    pub(crate) code: Vec<u8>,
    pub(crate) lines: Vec<usize>,
    pub(crate) constants: Vec<Value>,
}

// todo: remove this when all operations have been fixed
#[allow(dead_code)]
pub mod prec {
    #[repr(usize)]
    enum Prec {
        None,
        Assignment,
        Or,
        And,
        Equality,
        Comparison,
        Term,
        Factor,
        Unary,
        Call,
        Primary,
    }
    use Prec::*;

    pub const NONE: usize = None as usize;
    pub const ASSIGNMENT: usize = Assignment as usize;
    pub const OR: usize = Or as usize;
    pub const AND: usize = And as usize;
    pub const EQUALITY: usize = Equality as usize;
    pub const COMPARISON: usize = Comparison as usize;
    pub const TERM: usize = Term as usize;
    pub const FACTOR: usize = Factor as usize;
    pub const UNARY: usize = Unary as usize;
    pub const CALL: usize = Call as usize;
    pub const PRIMARY: usize = Primary as usize;
}

struct ParseRule {
    prefix_rule: Option<ParseFn>,
    infix_rule: Option<ParseFn>,
    precedence: usize,
}

type ParseFn = fn(&mut Compiler, bool);

use bigdecimal::BigDecimal;
use bigdecimal::Num;
use bigdecimal::ToPrimitive;

impl<'a> Compiler<'a> {
    /// Create a new compiler with empty source and no errors.
    ///
    /// This method also initializes the scanner and parser, and
    /// is fine to use any time we need a new [Compiler]
    pub fn new(vm: &mut VM) -> Compiler {
        let entry_point = Function {
            name: "".to_string(),
            arity: 0,
            chunk: Chunk {
                code: Vec::new(),
                lines: Vec::new(),
                constants: Vec::new(),
            },
        };
        let function = vm.memory.allocate(Object::Function(Box::new(entry_point)));
        Compiler {
            vm,
            function,
            ancestors: Vec::new(),
            scanner: Scanner::default(),
            parser: Parser {
                current: Token::default(),
                previous: Token::default(),
                panic_mode: false,
            },
            scope_depth: 0,
            locals: Vec::new(),
            error_chain: LoxErrorChain::default(),
        }
    }

    /// Compile the statement given and report errors back to the VM.
    ///
    /// The statement passed in can be a group of statements separated
    /// by a ';' character, as specified in Lox grammar.
    pub fn compile(mut self, statement: &str) -> Result<MemoryEntry, LoxErrorChain> {
        self.scanner.take_str(statement);

        self.advance();

        while !self.match_(Eof) {
            self.declaration();
        }

        self.end_compiler()
    }

    /// Move the parser forward by one token.
    ///
    /// This calls out to the scanner (which the parser owns) to
    /// scan through the source, passing whitespace, until it has scanned
    /// a single token.
    fn advance(&mut self) {
        std::mem::swap(&mut self.parser.previous, &mut self.parser.current);

        loop {
            self.parser.current = self.scanner.scan_token();

            if self.parser.current.type_ != Error {
                break;
            }

            for err in self.scanner.error_chain.errors() {
                self.error_chain.register(err);
            }
        }
    }

    /// If the current (next to be compiled) token matches, then advance
    ///
    /// Return whether the token was matched, and the skip-ahead
    /// behavior is sort of hidden from the caller. But that lets the
    /// caller do cool things like the following:
    ///
    /// ```not-public
    /// let vm = VM::default();
    /// let compiler = Compiler::new(&vm);
    /// if compiler.match_(Eof) {
    ///     println!("End of file");
    /// }
    /// ```
    fn match_(&mut self, type_: TokenType) -> bool {
        let matches = self.check(type_);

        if matches {
            self.advance();
        }

        matches
    }

    /// Test the current token type, used to simplify this check.
    fn check(&self, type_: TokenType) -> bool {
        self.parser.current.type_ == type_
    }

    /// Compile a declaration, or a lower precedence statement.
    ///
    /// This is the top-level definition in Lox's grammar.
    ///
    /// ```lox-grammar
    ///  declaration -> class_declaration
    ///                 | fun_declaration
    ///                 | var_declaration
    ///                 | statement`
    /// ```
    fn declaration(&mut self) {
        if self.match_(Class) {
            self.class_declaration();
        } else if self.match_(Fun) {
            self.fun_declaration();
        } else if self.match_(Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.parser.panic_mode {
            self.synchronize();
        }
    }

    /// Compile a variable declaration.
    ///
    /// The variable declaration syntax is:
    ///
    /// `var_declaration -> 'var' variable_name [ '=' expression ] ';'`
    ///
    /// An example would be: `var x = 10;`
    ///
    /// The variable declaration compiles down to the following bytecode
    /// in the case of a global variable definition, for example.
    ///
    /// ```lox
    /// var x;
    /// ```
    ///
    /// ```lox-bytecode
    /// Constants:
    ///     1: "x"
    /// Code:
    ///     op::Nil
    ///     op::DefineGlobal
    ///     1
    /// ```
    fn var_declaration(&mut self) {
        let name = self.parse_variable("Expect variable name.");

        if self.match_(Equal) {
            self.expression();
        } else {
            self.emit_byte(op::NIL);
        }
        self.consume(Semicolon, "Expect ';' after variable declaration.");

        self.define_variable(name);
    }

    /// Define either a local or global variable depending on the scope.
    ///
    /// Emits bytecode for variable definition, assuming the bytecode prior to
    /// this leaves that value on the stack.
    fn define_variable(&mut self, name: u8) {
        if self.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.emit_bytes(op::DEFINE_GLOBAL, name);
    }

    fn mark_initialized(&mut self) {
        self.locals.last_mut().unwrap().depth = self.scope_depth;
    }

    fn consume(&mut self, token_type: TokenType, message: &str) {
        if self.parser.current.type_ == token_type {
            self.advance();
        } else {
            self.error_at_current(message);
        }
    }

    fn expression(&mut self) {
        // Called parsePrecedence in the book
        self.expression_with_precedence(prec::ASSIGNMENT);
    }

    fn emit_bytes(&mut self, op1: u8, op2: u8) {
        self.emit_byte(op1);
        self.emit_byte(op2);
    }

    fn emit_byte(&mut self, op: u8) {
        let line = self.parser.previous.line;
        self.function().chunk.lines.push(line);
        self.function().chunk.code.push(op)
    }

    fn emit_return(&mut self) {
        self.emit_byte(op::RETURN)
    }

    fn parse_variable(&mut self, message: &str) -> u8 {
        self.consume(Identifier, message);

        self.declare_variable();
        if self.scope_depth > 0 {
            return 0;
        }

        self.identifier_constant()
    }

    fn declare_variable(&mut self) {
        if self.scope_depth == 0 {
            // Don't declare global variables. They are resolved dynamically
            // at runtime
            return;
        }

        let mut error = false;
        for local in &self.locals {
            if local.depth != UNINITIALIZED && local.depth < self.scope_depth {
                // There is a variable in the local stack that's not uninitialized but
                // is a level above the current set of locals (in terms of block scoping)
                break;
            }

            if self.prev_identifier_equals(&local.name) {
                error = true;
            }
        }
        self.add_local();

        if error {
            self.error("Already a variable with this name in this scope.");
        }
    }

    fn prev_identifier_equals(&self, token: &Token) -> bool {
        if self.parser.previous.length != token.length {
            false
        } else {
            for ch in 0..self.parser.previous.length {
                let ch1 = self.char_at(self.parser.previous.start + ch);
                let ch2 = self.char_at(token.start + ch);
                if ch1 != ch2 {
                    return false;
                }
            }
            true
        }
    }

    fn char_at(&self, ch: usize) -> &char {
        self.scanner.source.get(ch).unwrap()
    }

    fn add_local(&mut self) {
        let len = self.locals.len() as isize;
        if len >= 256 {
            self.error("Too many local variables.");
        }

        self.locals.push(Local {
            // todo: is clone expensive here?
            // Shouldn't matter because we're compiling
            // but it would be nice to check
            name: self.parser.previous.clone(),
            depth: len,
            is_captured: false,
        })
    }

    fn identifier_constant(&mut self) -> u8 {
        let obj_pointer = self.copy_string();
        self.make_constant(Value::Object(obj_pointer))
    }

    fn class_declaration(&self) {
        todo!("compile a class definition along with methods")
    }

    fn fun_declaration(&self) {
        todo!("compile a single function definition")
    }

    fn statement(&mut self) {
        if self.match_(Print) {
            self.print_statement();
        } else if self.match_(Assert) {
            self.assert_statement();
        } else if self.match_(For) {
            self.for_statement();
        } else if self.match_(If) {
            self.if_statement();
        } else if self.match_(Return) {
            self.return_statement();
        } else if self.match_(While) {
            self.while_statement();
        } else if self.match_(LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.epxression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(Semicolon, "Expect ';' after print statement.");
        self.emit_byte(op::PRINT);
    }

    fn assert_statement(&mut self) {
        self.expression();
        self.consume(Semicolon, "Expect ';' after assertion statement.");
        self.emit_byte(op::ASSERT);
    }

    fn return_statement(&self) {
        todo!("compile a return statement")
    }

    fn if_statement(&mut self) {
        self.consume(LeftParen, "Expect '(' after if keyword.");
        self.expression();
        self.consume(RightParen, "Expect ')' after if condition.");

        // The if branch starts after the jump instruction, which is three bytes,
        // which would put it two bytes ahead of the `len` since len is one more
        // than the current "ip".
        //
        // We track the actual location of the jump instruction
        let then_jump = self.function().chunk.code.len();

        self.emit_byte(op::JUMP_IF_FALSE);
        self.emit_bytes(0, 0); // We'll patch these after the else block

        // Emit bytecode for the then branch, use statement so we don't have to
        // consume a block: this could just be a single expression statement
        self.statement();

        if !self.match_(Else) {
            let end = self.function().chunk.code.len();
            self.patch_jump(then_jump, end - (then_jump + 2));
            return;
        }

        // The else branch is where we jump to, so we'll use the current "ip" to
        // patch the if branch, and we'll patch this jump instruction once we've
        // emitted he else branch.
        let else_jump = self.function().chunk.code.len();
        self.patch_jump(then_jump, (else_jump + 3) - (then_jump + 2));

        self.emit_byte(op::JUMP);
        self.emit_bytes(0, 0); // We'll patch these after the whole if/else

        // Emit bytecode for else branch, same reasoning as then branch
        self.statement();

        // The first instruction executed outside the loop
        let end = self.function().chunk.code.len();

        self.patch_jump(else_jump, end - (else_jump + 2));
    }

    fn while_statement(&mut self) {
        let pre_condition = self.function().chunk.code.len();

        self.consume(LeftParen, "Expect '(' after while keyword.");
        self.expression();
        self.consume(RightParen, "Expect ')' after while condition.");

        let condition_jump = self.function().chunk.code.len();

        self.emit_byte(op::JUMP_IF_FALSE);
        self.emit_bytes(0, 0);

        self.statement();

        let loop_patch = self.function().chunk.code.len();

        self.emit_byte(op::LOOP);
        self.emit_bytes(0, 0);

        self.patch_jump(loop_patch, (loop_patch + 2) - pre_condition);
        self.patch_jump(condition_jump, (loop_patch + 3) - (condition_jump + 2));
    }

    fn for_statement(&self) {
        todo!("compile a for loop")
    }

    fn jump_target(ip: usize) -> (u8, u8) {
        let byte1 = (ip >> 8) & 0xff;
        let byte1 = byte1 as u8;

        let byte2 = ip & 0xff;
        let byte2 = byte2 as u8;

        (byte1, byte2)
    }

    /// Ammend a jump instruction's two bytes for target with the location
    /// in the source code that it will jump to.
    ///
    /// `Patch_location` is the location in the source that the jump is
    /// located. This should be the index in the bytecode of the actual
    /// jump instruction, i.e. the instructions to be patched will be
    /// (location + 1) and (location + 2). You can get this value by
    /// measuring the length of the bytecode array just before emitting
    /// a jump instruction.
    ///
    /// `Jump_target` is the instruction we want to patch to. This should
    /// point to the index in the bytecode that we want to set the `ip`
    /// to, the next instruction to execute. You can get this value by
    /// measuring the length of the bytecode just before you emit the
    /// instruction to jump to, or by adding three to the measured
    /// `patch_location` for a jump instruction you want to jump past.
    fn patch_jump(&mut self, patch_location: usize, ip_inc: usize) {
        let (byte1, byte2) = Self::jump_target(ip_inc);
        self.function().chunk.code[patch_location + 1] = byte1;
        self.function().chunk.code[patch_location + 2] = byte2;
    }

    fn epxression_statement(&mut self) {
        self.expression();
        self.consume(Semicolon, "Expect ';' after expression.");
        self.emit_byte(op::POP);
    }

    fn block(&mut self) {
        while !self.check(RightBrace) && !self.check(Eof) {
            self.declaration()
        }

        self.consume(RightBrace, "Unmatched '{' results in unterminated block.")
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        // Now iterate through all the local variables and
        // pop them from the stack if they're at this scope
        loop {
            if self.locals.is_empty() {
                return;
            }
            if self.locals.last().unwrap().depth <= self.scope_depth {
                return;
            }
            self.emit_byte(op::POP);
            self.locals.pop();
        }
    }

    fn expression_with_precedence(&mut self, precedence: usize) {
        let precedence = precedence;
        self.advance();

        let prefix_rule: Option<ParseFn> = self.previous_rule().prefix_rule;
        let can_assign = precedence <= prec::ASSIGNMENT;

        match prefix_rule {
            Some(rule) => rule(self, can_assign),
            None => self.error("Expect expression."),
        }

        while precedence <= self.current_rule().precedence {
            self.advance();
            if let Some(infix_rule) = self.previous_rule().infix_rule {
                infix_rule(self, can_assign);
            }
        }

        if can_assign && self.match_(Equal) {
            self.error("Invalid assignment target.");
        }
    }

    fn previous_rule(&self) -> ParseRule {
        self.get_rule(&self.parser.previous.type_)
    }

    fn current_rule(&self) -> ParseRule {
        self.get_rule(&self.parser.current.type_)
    }

    fn get_rule(&self, type_: &TokenType) -> ParseRule {
        match type_ {
            And => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: prec::AND,
            },
            Assert => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            Bang => ParseRule {
                prefix_rule: Some(unary),
                infix_rule: None,
                precedence: prec::NONE,
            },
            BangEqual => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: prec::EQUALITY,
            },
            Class => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            Comma => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            Dot => ParseRule {
                prefix_rule: None,
                infix_rule: Some(dot),
                precedence: prec::CALL,
            },
            Else => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            Eof => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            Equal => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            EqualEqual => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: prec::EQUALITY,
            },
            Error => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            False => ParseRule {
                prefix_rule: Some(literal),
                infix_rule: None,
                precedence: prec::PRIMARY,
            },
            For => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            Fun => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            Greater => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: prec::EQUALITY,
            },
            GreaterEqual => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: prec::EQUALITY,
            },
            Identifier => ParseRule {
                prefix_rule: Some(variable),
                infix_rule: None,
                precedence: prec::NONE,
            },
            If => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            LeftBrace => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            LeftParen => ParseRule {
                prefix_rule: Some(grouping),
                infix_rule: None,
                precedence: prec::NONE,
            },
            Less => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: prec::EQUALITY,
            },
            LessEqual => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: prec::EQUALITY,
            },
            Minus => ParseRule {
                prefix_rule: Some(unary),
                infix_rule: Some(binary),
                precedence: prec::TERM,
            },
            TokenNil => ParseRule {
                prefix_rule: Some(literal),
                infix_rule: None,
                precedence: prec::NONE,
            },
            TokenNumber => ParseRule {
                prefix_rule: Some(number),
                infix_rule: None,
                precedence: prec::ASSIGNMENT,
            },
            Or => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: prec::OR,
            },
            Plus => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: prec::TERM,
            },
            Print => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            Return => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            RightBrace => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            RightParen => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            Semicolon => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            Slash => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: prec::FACTOR,
            },
            Star => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: prec::FACTOR,
            },
            TokenString => ParseRule {
                prefix_rule: Some(string),
                infix_rule: None,
                precedence: prec::ASSIGNMENT,
            },
            Super => ParseRule {
                prefix_rule: Some(super_),
                infix_rule: None,
                precedence: prec::NONE,
            },
            This => ParseRule {
                prefix_rule: Some(this_),
                infix_rule: None,
                precedence: prec::NONE,
            },
            True => ParseRule {
                prefix_rule: Some(literal),
                infix_rule: None,
                precedence: prec::PRIMARY,
            },
            Var => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
            While => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: prec::NONE,
            },
        }
    }

    fn copy_string(&mut self) -> MemoryEntry {
        let token = &self.parser.previous;
        let string = Object::String(self.scanner.copy_segment(token));
        self.vm.memory.allocate(string)
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        // Check if the exact constant is in the constant table -- this compares
        // memory locations, so objects that are equal will not count here
        for (i, constant) in self.function().chunk.constants.iter().enumerate() {
            if constant.strict_equals(&value) {
                return i as u8;
            }
        }

        // If it's not a string then that's fine, just continue as normal
        if self.function().chunk.constants.len() >= 256 {
            let token = self.copy_string();
            let token = self.vm.memory.retrieve(&token).as_string().clone();
            self.error(&format!("Too many constants ({}).", token));
            0
        } else {
            self.function().chunk.constants.push(value);
            (self.function().chunk.constants.len() - 1) as u8
        }
    }

    fn error_at_current(&mut self, message: &str) {
        let message = message.to_string();
        self.error_chain.register(ParseError {
            line: self.parser.current.line,
            message,
        })
    }

    fn error(&mut self, message: &str) {
        let message = message.to_string();
        self.error_chain.register(ParseError {
            line: self.parser.previous.line,
            message,
        })
    }

    fn synchronize(&mut self) {
        // If there's an error, we'll skip ahead to the end of the statement/expression,
        // or if the error happened at the last expression in a block, we look for the
        // end of the block but don't consume it (that lets us compile the block correctly
        // so that we can continue compiling the rest of the file)
        while !self.match_(Semicolon) && !self.check(RightBrace) {
            self.advance();
        }
    }

    fn end_compiler(mut self) -> Result<MemoryEntry, LoxErrorChain> {
        self.emit_return();

        self.function().disassemble_chunk();
        self.function().graph_output_chunk();

        if self.scanner.error_chain.had_error() {
            self.scanner.error_chain.print_all();
        }

        if self.error_chain.had_error() {
            Err(self.error_chain)
        } else {
            Ok(self.function)
        }
    }

    fn function(&mut self) -> &mut Function {
        self.vm
            .memory
            .retrieve_mut(&self.function)
            .as_mut_function()
    }

    fn named_variable(&mut self, can_assign: bool) {
        // Local variable
        let arg: isize = self.resolve_local();
        if arg > 0 && self.match_(TokenType::Equal) {
            // UNINITIALIZED (-1) is the only negative value
            self.expression();
            return self.emit_bytes(op::SET_LOCAL, arg as u8);
        } else if arg > 0 {
            return self.emit_bytes(op::GET_LOCAL, arg as u8);
        }

        // Global variable
        let arg = self.identifier_constant();

        if can_assign && self.match_(TokenType::Equal) {
            self.expression();
            self.emit_bytes(op::SET_GLOBAL, arg)
        } else {
            self.emit_bytes(op::GET_GLOBAL, arg)
        }
    }

    fn resolve_local(&mut self) -> isize {
        for (i, local) in self.locals.iter().enumerate() {
            if self.prev_identifier_equals(&local.name) {
                return (i + 1) as isize; // Always -1 or 1..256
            }
        }

        UNINITIALIZED
    }
}

/// Section: parse functions
fn number(this: &mut Compiler, _can_assign: bool) {
    let value = this.scanner.copy_segment(&this.parser.previous).convert();

    let index = this.make_constant(Value::Number(value));
    this.emit_bytes(op::CONSTANT, index);
}

/// Same logic for all binary operators such as plus, times etc.
///
/// The strategy is remember the operator, parse an expression to
/// the right with strictly higher precedence (plus will parse times)
/// and then emit a binary operator that will pull the left and
/// right values off the stack.
fn binary(this: &mut Compiler, _can_assign: bool) {
    let token = this.parser.previous.type_.clone();
    let rule = this.get_rule(&token);
    // To safely call this, you must guarantee that there are
    // no parse rules that produce binary with the highest
    // precedence (PrecPrimary)
    let prec = rule.precedence + 1;

    this.expression_with_precedence(prec);

    match token {
        And => this.emit_byte(op::AND),
        BangEqual => this.emit_bytes(op::EQUAL, op::NOT),
        EqualEqual => this.emit_byte(op::EQUAL),
        Greater => this.emit_byte(op::GREATER),
        GreaterEqual => this.emit_byte(op::GREATER_EQUAL),
        Less => this.emit_bytes(op::GREATER_EQUAL, op::NOT),
        LessEqual => this.emit_bytes(op::GREATER, op::NOT),
        Minus => this.emit_byte(op::SUBTRACT),
        Or => this.emit_byte(op::OR),
        Plus => this.emit_byte(op::ADD),
        Slash => this.emit_byte(op::DIVIDE),
        Star => this.emit_byte(op::MULTIPLY),
        _ => this.error_at_current("Impossible binary operator. (this is an interpreter bug)"),
    }
}

fn unary(this: &mut Compiler, _can_assign: bool) {
    let token = this.parser.previous.type_.clone();
    let rule = this.get_rule(&token);
    let prec = rule.precedence + 1;

    this.expression_with_precedence(prec);

    match token {
        Bang => this.emit_byte(op::NOT),
        Minus => this.emit_byte(op::NEGATE),
        _ => this.error_at_current("Impossible unary operator. (this is an interpreter bug)"),
    }
}

fn grouping(this: &mut Compiler, _can_assign: bool) {
    this.expression();
    this.consume(RightParen, "Expected closing ')'.");
}

fn string(this: &mut Compiler, _can_assign: bool) {
    let value = this.copy_string();

    let index = this.make_constant(Value::Object(value));
    this.emit_bytes(op::CONSTANT, index);
}

fn literal(this: &mut Compiler, _can_assign: bool) {
    let token = &this.parser.previous.type_;
    let index = match token {
        True => this.make_constant(Value::Boolean(true)),
        False => this.make_constant(Value::Boolean(false)),
        TokenNil => this.make_constant(Value::Nil),
        _ => panic!("Internal error: cannot make literal (this is a bug)."),
    };

    this.emit_bytes(op::CONSTANT, index);
}

fn this_(this: &mut Compiler, _can_assign: bool) {
    todo!("Compile a this expression for {:?}", this)
}

fn super_(this: &mut Compiler, _can_assign: bool) {
    todo!("Compile a super expression for {:?}", this)
}

fn variable(this: &mut Compiler, can_assign: bool) {
    this.named_variable(can_assign);
}

fn dot(this: &mut Compiler, _can_assign: bool) {
    todo!("Compile a dot expression for {:?}", this)
}

trait ConvertNumber {
    fn convert(&self) -> f64;
}

impl ConvertNumber for String {
    fn convert(&self) -> f64 {
        BigDecimal::from_str_radix(self, 10)
            .unwrap()
            .to_f64()
            .unwrap()
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::{debug::Disassembler, vm::LoxError::ScanError};

    macro_rules! compile_expression {
        ($bytecode:ident, $vm:ident, $name:ident, $text:literal, $test_case:expr) => {
            #[test]
            fn $name() {
                let mut $vm = VM::default();
                let compiler = Compiler::new(&mut $vm);

                let $vm = match compiler.compile($text) {
                    Ok(_) => {
                        $vm.memory
                            .retrieve(&crate::object::mem(0))
                            .as_function()
                            .disassemble_chunk();
                        $vm
                    }
                    Err(e) => {
                        println!("Error in test: {}", e);
                        panic!("Failing test: expected code to compile.")
                    }
                };
                let $bytecode = $vm.memory.retrieve(&crate::object::mem(0)).as_function();

                assert_eq!($bytecode.chunk.code, $test_case)
            }
        };
    }

    macro_rules! compile_broken {
        ($errors:ident, $name:ident, $text:literal, $test_case:expr) => {
            #[test]
            fn $name() {
                let mut vm = VM::default();
                let compiler = Compiler::new(&mut vm);

                let mut err = compiler.compile($text).unwrap_err();
                let mut $errors = err.errors();

                $test_case
            }
        };
    }

    #[test]
    fn constants_in_variable_declaration() {
        let mut vm = VM::default();
        let compiler = Compiler::new(&mut vm);
        let fun = compiler.compile("var x;").unwrap();
        let bytecode = vm.memory.retrieve(&fun).as_function();

        assert_eq!(1, bytecode.chunk.constants.len());

        if let Value::Object(ptr) = bytecode.chunk.constants.get(0).unwrap() {
            if let Object::String(s) = vm.memory.retrieve(ptr) {
                assert_eq!(**s, "x");
            } else {
                panic!("expected memory to contian string variable name")
            }
        } else {
            panic!("expected the constant table to point to memory")
        }
    }

    compile_expression! {
        bytecode, vm,
        compile_variable_declaration,
        "var x;",
        vec![op::NIL, op::DEFINE_GLOBAL, 0, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        compile_simple_integer_expression,
        "1;",
        vec![op::CONSTANT, 0, op::POP, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        compile_print_expression,
        "print 1;",
        vec![op::CONSTANT, 0, op::PRINT, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        compile_print_string,
        "print \"hello\";",
        vec![op::CONSTANT, 0, op::PRINT, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        add_two_numbers,
        "1+1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 0, op::ADD, op::POP, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        subtract_two_numbers,
        "1-1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 0, op::SUBTRACT, op::POP, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        multiply_two_numbers,
        "1*1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 0, op::MULTIPLY, op::POP, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        divide_two_numbers,
        "1/1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 0, op::DIVIDE, op::POP, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        negate_a_number,
        "-1;",
        vec![op::CONSTANT, 0, op::NEGATE, op::POP, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        compare_for_equality,
        "1 == 1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 0, op::EQUAL, op::POP, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        greater_than_numbers,
        "2 > 1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 1, op::GREATER, op::POP, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        greater_than_equal_numbers,
        "2 >= 1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 1, op::GREATER_EQUAL, op::POP, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        less_than_numbers,
        "2 < 1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 1, op::GREATER_EQUAL, op::NOT, op::POP, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        less_than_equal_numbers,
        "2 <= 1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 1, op::GREATER, op::NOT, op::POP, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        declare_variable,
        "var x;",
        vec![op::NIL, op::DEFINE_GLOBAL, 0, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        define_global_variable,
        "var x = 10;",
        vec![op::CONSTANT, 1, op::DEFINE_GLOBAL, 0, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        define_and_reference_global_variable,
        "
            var x = 10;
            x;
        ",
        vec![op::CONSTANT, 1, op::DEFINE_GLOBAL, 0, op::GET_GLOBAL, 0, op::POP, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        simple_block_scope,
        "{ true; }",
        vec![op::CONSTANT, 0, op::POP, op::RETURN]
    }

    compile_expression! {
        bytecode, vm,
        block_scope_locals,
        "
            var x = 10;
            {
                var x = 20;
            }
            {
                var x = 30;
            }
        ",
        vec![
            op::CONSTANT,
            1,
            op::DEFINE_GLOBAL,
            0,
            op::CONSTANT,
            2,
            op::POP,
            op::CONSTANT,
            3,
            op::POP,
            op::RETURN
        ]
    }

    compile_expression! {
        bytecode, vm,
        block_scope_locals_get_and_set,
        "
            {
                var x;
                x = 10;
                x;
            }
        ",
        vec![
            op::NIL,
            op::CONSTANT,
            0,
            op::SET_LOCAL,
            1,
            op::POP,
            op::GET_LOCAL,
            1,
            op::POP,
            op::POP,
            op::RETURN,
        ]
    }

    compile_expression! {
        bytecode, vm,
        if_statement,
        "
            if (true) print 10;
            else print 20;
        ",
        vec![
            op::CONSTANT,
            0,
            op::JUMP_IF_FALSE,
            0,
            7,
            op::CONSTANT,
            1,
            op::PRINT,
            op::JUMP,
            0,
            4,
            op::CONSTANT,
            2,
            op::PRINT,
            op::RETURN
        ]
    }

    compile_expression! {
        bytecode, vm,
        if_statement_no_else,
        "
            if (true) print 10;
        ",
        vec![
            op::CONSTANT,
            0,
            op::JUMP_IF_FALSE,
            0,
            4,
            op::CONSTANT,
            1,
            op::PRINT,
            op::RETURN
        ]
    }

    compile_expression! {
        bytecode, vm,
        if_statement_with_block,
        "
            if (true) {
                var x = 10;
            }
        ",
        vec![
            op::CONSTANT,
            0,
            op::JUMP_IF_FALSE,
            0,
            4,
            op::CONSTANT,
            1,
            op::POP,
            op::RETURN
        ]
    }

    compile_expression! {
        bytecode, vm,
        while_statement,
        "
            while (false) {
                var x = 10;
            }
        ",
        vec![
            op::CONSTANT,
            0,
            op::JUMP_IF_FALSE,
            0,
            7,
            op::CONSTANT,
            1,
            op::POP,
            op::LOOP,
            0,
            10,
            op::RETURN
        ]
    }

    compile_broken! {
        errors,
        compile_error_incomplete_var_expression,
        "var;",
        {
            assert!(matches!(errors.pop().unwrap(), ParseError { .. }));
            assert!(matches!(errors.pop(), None));
        }
    }

    compile_broken! {
        errors,
        compile_error_unexpected_end_of_expr,
        ";",
        {
            assert_eq!(2, errors.len());

            let expr_err = errors.pop().unwrap();
            let semi_err = errors.pop().unwrap();

            assert!(matches!(&expr_err, ParseError { .. }));
            assert!(matches!(&semi_err, ParseError { .. }));
        }
    }

    compile_broken! {
        errors,
        compile_error_unterminated_string,
        "\"a",
        {
            assert_eq!(1, errors.len());
            assert!(matches!(errors.pop().unwrap(), ScanError { .. }));
        }
    }
}
