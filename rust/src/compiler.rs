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
    scanner: &'a mut Scanner,
    parser: &'a mut Parser,

    parent: Option<usize>,

    function: MemoryEntry,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,

    scope_depth: isize,

    error_chain: LoxErrorChain,
}

#[derive(Debug)]
pub struct Local {
    name: Token,
    depth: isize,
    is_captured: bool,
}

#[derive(Debug)]
pub struct Upvalue {
    index: u8,
    is_local: bool,
}

/// The parser that does all the work creating the bytecode.
#[derive(Debug)]
pub struct Parser {
    current: Token,
    previous: Token,

    panic_mode: bool,
}

impl Default for Parser {
    fn default() -> Self {
        Self {
            current: Token::default(),
            previous: Token::default(),
            panic_mode: false,
        }
    }
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
    type_: Type,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
enum Type {
    Script,
    Closure,
    Method,
    Initializer,
}

#[derive(Debug)]
pub(crate) struct Chunk {
    pub(crate) code: Vec<u8>,
    pub(crate) lines: Vec<usize>,
    pub(crate) constants: Vec<Value>,
}

impl Default for Chunk {
    fn default() -> Self {
        Self {
            code: Vec::new(),
            lines: Vec::new(),
            constants: Vec::new(),
        }
    }
}

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

fn parse_rule(
    prefix_rule: Option<ParseFn>,
    infix_rule: Option<ParseFn>,
    precedence: usize,
) -> ParseRule {
    ParseRule {
        prefix_rule,
        infix_rule,
        precedence,
    }
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
    pub fn new(scanner: &'a mut Scanner, parser: &'a mut Parser, vm: &'a mut VM) -> Compiler<'a> {
        let name = "".to_string();
        let entry_point = Function {
            name,
            arity: 0,
            chunk: Chunk::default(),
            type_: Type::Script,
        };
        let function = vm.memory.allocate(Object::Function(Box::new(entry_point)));
        Compiler {
            vm,
            function,
            scanner,
            parser,
            parent: None,
            scope_depth: 0,
            locals: Vec::new(),
            upvalues: Vec::new(),
            error_chain: LoxErrorChain::default(),
        }
    }

    pub fn spawn(&mut self, name: MemoryEntry) -> Compiler {
        let name = self.vm.get_string(name).clone();
        let parent: *mut Compiler<'a> = self;
        let parent: usize = parent as usize; // this is honestly kinda stupid
        let function = Function {
            name,
            arity: 0,
            chunk: Chunk::default(),
            type_: Type::Closure,
        };
        let function = self
            .vm
            .memory
            .allocate(Object::Function(Box::new(function)));
        Compiler {
            function,
            vm: self.vm,
            scanner: self.scanner,
            parser: self.parser,
            parent: Some(parent),
            scope_depth: self.scope_depth + 1,
            locals: Vec::new(),
            upvalues: Vec::new(),
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
            self.declaration()?;
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
    fn declaration(&mut self) -> Result<(), LoxErrorChain> {
        if self.match_(Class) {
            self.class_declaration();
        } else if self.match_(Fun) {
            self.fun_declaration()?;
        } else if self.match_(Var) {
            self.var_declaration();
        } else {
            self.statement()?;
        }

        if self.parser.panic_mode {
            self.synchronize();
        }

        Ok(())
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
        if self.scope_depth == 0 {
            return;
        }
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
        self.function_mut().chunk.lines.push(line);
        self.function_mut().chunk.code.push(op)
    }

    fn emit_jump(&mut self, op: u8) {
        self.emit_byte(op);
        self.emit_bytes(0, 0); // Patch these later
    }

    fn emit_return(&mut self) {
        self.emit_bytes(op::NIL, op::RETURN)
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

    fn fun_declaration(&mut self) -> Result<(), LoxErrorChain> {
        let name = self.parse_variable("Expect function name after fun keywoard");
        self.mark_initialized();
        self.function_body()?;
        self.define_variable(name);
        Ok(())
    }

    fn function_body(&mut self) -> Result<MemoryEntry, LoxErrorChain> {
        let name = self.copy_string();
        let mut compiler = self.spawn(name);

        compiler.function_parameters();
        compiler.consume(LeftBrace, "Expected '{' before function body.");
        compiler.block()?;

        let upvalues = compiler.upvalues.len() as u8;
        let function = compiler.end_compiler()?;

        let function_constant = self.make_constant(Value::Object(function));
        self.emit_byte(op::CLOSURE);
        self.emit_bytes(function_constant, upvalues);

        Ok(function)
    }

    fn function_parameters(&mut self) {
        self.consume(LeftParen, "Expected '(' after function name.");

        if self.match_(RightParen) {
            return;
        }

        'parameters: loop {
            self.consume(Identifier, "Expected function parameter");
            self.add_local();
            self.function_mut().arity += 1;

            if !self.match_(Comma) {
                break 'parameters;
            }
        }

        self.consume(RightParen, "Expected ')' after '(' in function call.")
    }

    fn function_call(&mut self) {
        if self.match_(RightParen) {
            self.emit_bytes(op::CALL, 0);
            return;
        }

        let mut args = 0;
        'parameters: loop {
            if args == u8::MAX {
                return self.error("Too many arguments.");
            }
            self.expression();
            args += 1;

            if !self.match_(Comma) {
                break 'parameters;
            }
        }

        self.emit_bytes(op::CALL, args);
        self.consume(RightParen, "Expect ')' after function parameters.")
    }

    fn statement(&mut self) -> Result<(), LoxErrorChain> {
        if self.match_(Print) {
            self.print_statement();
        } else if self.match_(Assert) {
            self.assert_statement();
        } else if self.match_(For) {
            self.for_statement()?;
        } else if self.match_(If) {
            self.if_statement()?;
        } else if self.match_(Return) {
            self.return_statement();
        } else if self.match_(While) {
            self.while_statement()?;
        } else if self.match_(LeftBrace) {
            self.begin_scope();
            self.block()?;
            self.end_scope();
        } else {
            self.epxression_statement();
        }

        Ok(())
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

    fn return_statement(&mut self) {
        if self.function().type_ == Type::Script {
            return self.error("Cannot return from top level.");
        }
        if self.match_(Semicolon) {
            return self.emit_return();
        }

        self.expression();
        self.consume(Semicolon, "Expect ';' after return statement;");
        self.emit_byte(op::RETURN);
    }

    fn if_statement(&mut self) -> Result<(), LoxErrorChain> {
        self.consume(LeftParen, "Expect '(' after if keyword.");
        self.expression();
        self.consume(RightParen, "Expect ')' after if condition.");

        let else_jump = self.next_instruction();
        self.emit_jump(op::JUMP_IF_FALSE); // We'll patch these after the else block
        let else_jump_from = self.next_instruction();

        // Emit bytecode for the then branch, use statement so we don't have to
        // consume a block: this could just be a single expression statement
        self.statement()?;

        if !self.match_(Else) {
            let else_jump_to = self.next_instruction();
            self.patch_jump(else_jump, else_jump_to - else_jump_from);
            return Ok(());
        }

        let then_jump = self.next_instruction();
        self.emit_jump(op::JUMP); // We'll patch these after the whole if/else
        let else_jump_to = self.next_instruction();
        let then_jump_from = self.next_instruction();

        // The else branch is where we jump to, so we'll use the current `ip` to
        // patch the if branch, and we'll patch this jump instruction once we've
        // emitted he else branch. Note that the current `ip` at the time when
        // we execute the jump is just *after* the second jump byte. That's
        // because we've just read both bytes.
        //
        // This could go at the end of the method, but I'll leave it hear so it's
        // easier to read what's going on.
        self.patch_jump(else_jump, else_jump_to - else_jump_from);

        // Emit bytecode for else branch, same reasoning as then branch
        self.statement()?;

        // The first instruction executed outside the loop
        let then_jump_to = self.next_instruction();

        // Note we only need to execute the then jump if there is an else branch,
        // thus we return early above when if we don't match `Else`
        self.patch_jump(then_jump, then_jump_to - then_jump_from);

        Ok(())
    }

    fn while_statement(&mut self) -> Result<(), LoxErrorChain> {
        // The below is better understood as a modification to the if
        // statement, the comments are the same plus the loop.
        let loop_to = self.next_instruction();

        self.consume(LeftParen, "Expect '(' after while keyword.");
        self.expression();
        self.consume(RightParen, "Expect ')' after while condition.");

        let jump_patch = self.next_instruction();
        self.emit_jump(op::JUMP_IF_FALSE);
        let jump_from = self.next_instruction();

        self.statement()?;

        let loop_patch = self.next_instruction();
        self.emit_jump(op::LOOP);

        let loop_from = self.next_instruction();
        let jump_to = self.next_instruction();

        // Loop jumps are negated in the code, and we pass in a `usize`
        // which can't overflow negative, so we reverse the arguments.
        self.patch_jump(loop_patch, loop_from - loop_to);
        self.patch_jump(jump_patch, jump_to - jump_from);

        Ok(())
    }

    fn for_statement(&mut self) -> Result<(), LoxErrorChain> {
        // The loop iterator in a for statement act like they're in a
        // new block all their own
        self.begin_scope();

        // Begin consuming the for statements, stopping at each ';'
        self.consume(LeftParen, "Expect '(' after for keyword.");
        self.declaration()?;

        // Now we basically do a while loop with the next statement as the condition
        // but with an extra fancy jump added in there.
        let restart_loop_to = self.next_instruction();
        self.expression();
        self.consume(Semicolon, "Expect ';' after condition in for clause.");

        // If the condition was true, we jump out of the loop
        let exit_loop_patch = self.next_instruction();
        self.emit_jump(op::JUMP_IF_FALSE);
        let exit_loop_from = self.next_instruction();
        // Otherwise we jump forward to the start of the loop body.
        let start_body_patch = self.next_instruction();
        self.emit_jump(op::JUMP);
        let start_body_from = self.next_instruction();

        // The loop iterator update
        let update_iter_to = self.next_instruction();
        self.expression();
        self.emit_byte(op::POP);
        self.consume(RightParen, "Expect ')' after for clauses.");

        // Jump back to the beginning of the loop where we check the condition
        let restart_loop_patch = self.next_instruction();
        self.emit_jump(op::LOOP);
        let restart_loop_from = self.next_instruction();

        // Loop body
        let start_body_to = self.next_instruction();
        self.statement()?;

        // Jump back to the iterator update
        let update_iter_patch = self.next_instruction();
        self.emit_jump(op::LOOP);
        let update_iter_from = self.next_instruction();

        // Now we're done with the for bytecode, so this is where we jump to
        // when we exit the loop
        let exit_loop_to = self.next_instruction();

        // Now we patch all the jumps together so the rest of
        // the control flow is easier to follow.
        self.patch_jump(start_body_patch, start_body_to - start_body_from);
        self.patch_jump(update_iter_patch, update_iter_from - update_iter_to); // loop
        self.patch_jump(restart_loop_patch, restart_loop_from - restart_loop_to); // loop
        self.patch_jump(exit_loop_patch, exit_loop_to - exit_loop_from);

        self.end_scope();

        Ok(())
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
        self.function_mut().chunk.code[patch_location + 1] = byte1;
        self.function_mut().chunk.code[patch_location + 2] = byte2;
    }

    fn epxression_statement(&mut self) {
        self.expression();
        self.consume(Semicolon, "Expect ';' after expression.");
        self.emit_byte(op::POP);
    }

    fn block(&mut self) -> Result<(), LoxErrorChain> {
        while !self.check(RightBrace) && !self.check(Eof) {
            self.declaration()?;
        }

        self.consume(RightBrace, "Unmatched '{' results in unterminated block.");
        Ok(())
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
            And => parse_rule(None, Some(binary), prec::AND),
            Assert => parse_rule(None, None, prec::NONE),
            Bang => parse_rule(Some(unary), None, prec::NONE),
            BangEqual => parse_rule(None, Some(binary), prec::EQUALITY),
            Class => parse_rule(None, None, prec::NONE),
            Comma => parse_rule(None, None, prec::NONE),
            Dot => parse_rule(None, Some(dot), prec::CALL),
            Else => parse_rule(None, None, prec::NONE),
            Eof => parse_rule(None, None, prec::NONE),
            Equal => parse_rule(None, None, prec::NONE),
            EqualEqual => parse_rule(None, Some(binary), prec::EQUALITY),
            Error => parse_rule(None, None, prec::NONE),
            False => parse_rule(Some(literal), None, prec::PRIMARY),
            For => parse_rule(None, None, prec::NONE),
            Fun => parse_rule(None, None, prec::NONE),
            Greater => parse_rule(None, Some(binary), prec::COMPARISON),
            GreaterEqual => parse_rule(None, Some(binary), prec::COMPARISON),
            Identifier => parse_rule(Some(variable), None, prec::NONE),
            If => parse_rule(None, None, prec::NONE),
            LeftBrace => parse_rule(None, None, prec::NONE),
            LeftParen => parse_rule(Some(grouping), Some(call), prec::CALL),
            Less => parse_rule(None, Some(binary), prec::COMPARISON),
            LessEqual => parse_rule(None, Some(binary), prec::COMPARISON),
            Minus => parse_rule(Some(unary), Some(binary), prec::TERM),
            TokenNil => parse_rule(Some(literal), None, prec::NONE),
            TokenNumber => parse_rule(Some(number), None, prec::ASSIGNMENT),
            Or => parse_rule(None, Some(binary), prec::OR),
            Plus => parse_rule(None, Some(binary), prec::TERM),
            Print => parse_rule(None, None, prec::NONE),
            Return => parse_rule(None, None, prec::NONE),
            RightBrace => parse_rule(None, None, prec::NONE),
            RightParen => parse_rule(None, None, prec::NONE),
            Semicolon => parse_rule(None, None, prec::NONE),
            Slash => parse_rule(None, Some(binary), prec::FACTOR),
            Star => parse_rule(None, Some(binary), prec::FACTOR),
            TokenString => parse_rule(Some(string), None, prec::ASSIGNMENT),
            Super => parse_rule(Some(super_), None, prec::NONE),
            This => parse_rule(Some(this_), None, prec::NONE),
            True => parse_rule(Some(literal), None, prec::PRIMARY),
            Var => parse_rule(None, None, prec::NONE),
            While => parse_rule(None, None, prec::NONE),
        }
    }

    fn copy_string(&mut self) -> MemoryEntry {
        let token = &self.parser.previous;
        let string = Object::String(Box::new(self.scanner.copy_segment(token)));
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
            let token = self.vm.get_string(token).clone();
            self.error(&format!("Too many constants ({}).", token));
            0
        } else {
            self.function_mut().chunk.constants.push(value);
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
        while !self.match_(Semicolon) && !self.check(RightBrace) && !self.match_(Eof) {
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

    fn function_mut(&mut self) -> &mut Function {
        self.vm.get_function_mut(self.function)
    }

    fn function(&mut self) -> &Function {
        self.vm.get_function(self.function)
    }

    /// The location of the next instruction.
    ///
    /// This is a helper for creating and patching jumps. When called, it
    /// has the value of the instruction that will be run directly after the
    /// instruction that was last emitted. In the case of jumps, if you call
    /// this before emitting the jump, it points to the jump/loop itself.
    ///
    /// If you call this right after emitting a jump, it will point to
    /// whatever logic runs if the jump condition fails or whatever was
    /// emitted righ after the "body" of the previous jump (in the case
    /// of the unconditional jump of an if statement)
    fn next_instruction(&mut self) -> usize {
        self.function().chunk.code.len()
    }

    fn named_variable(&mut self, can_assign: bool) {
        // Local variable
        let arg: isize = self.resolve_local();
        if arg != UNINITIALIZED && self.match_(TokenType::Equal) {
            // UNINITIALIZED (-1) is the only negative value
            self.expression();
            return self.emit_bytes(op::SET_LOCAL, arg as u8);
        } else if arg != UNINITIALIZED {
            return self.emit_bytes(op::GET_LOCAL, arg as u8);
        }

        // Upvalue
        let arg = self.resolve_upvalue();
        if arg != UNINITIALIZED && self.match_(TokenType::Equal) {
            self.expression();
            return self.emit_bytes(op::SET_UPVALUE, arg as u8);
        } else if arg != UNINITIALIZED {
            return self.emit_bytes(op::GET_UPVALUE, arg as u8);
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

    fn resolve_upvalue(&mut self) -> isize {
        if self.parent.is_none() {
            return UNINITIALIZED;
        }
        let parent = self.parent.unwrap();

        unsafe {
            let parent: *mut Compiler = parent as *mut Compiler;
            let parent = parent.as_mut().unwrap();
            let local = parent.resolve_local();
            if local != UNINITIALIZED {
                let local = local as usize;
                parent.locals.get_mut(local).unwrap().is_captured = true;
                return self.add_upvalue(local, true) as isize;
            }

            let upvalue = parent.resolve_upvalue();
            if upvalue != UNINITIALIZED {
                let upvalue = upvalue as usize;
                return self.add_upvalue(upvalue, false) as isize;
            }
        }

        UNINITIALIZED
    }

    fn add_upvalue(&mut self, index: usize, is_local: bool) -> u8 {
        for (i, upvalue) in self.upvalues.iter().enumerate() {
            if upvalue.index == index as u8 && upvalue.is_local == is_local {
                return i as u8;
            }
        }

        if self.upvalues.len() == u8::MAX as usize {
            self.error("Too many closures in function.");
            return 0;
        }

        let index = index as u8;
        self.upvalues.push(Upvalue { is_local, index });
        (self.upvalues.len() - 1) as u8
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

    this.expression_with_precedence(prec::UNARY);

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

fn call(this: &mut Compiler, _can_assign: bool) {
    this.function_call();
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
    use crate::vm::LoxError::ScanError;

    macro_rules! test_program {
        ($name:ident, $text:literal, $test_case:expr) => {
            #[test]
            fn $name() {
                unsafe {
                    crate::debug::DEBUG_PRINT_CODE = true;
                    crate::debug::DEBUG_TRACE_EXECUTION = true;
                }
                let mut scanner = Scanner::default();
                let mut parser = Parser::default();
                let mut vm = VM::default();
                let compiler = Compiler::new(&mut scanner, &mut parser, &mut vm);

                println!("Compiling program:\n{}", $text);
                let vm = match compiler.compile($text) {
                    Ok(_) => vm,
                    Err(e) => {
                        println!("Error in test: {}", e);
                        panic!("Failing test: expected code to compile.")
                    }
                };
                let bytecode = &vm.get_function(crate::object::mem(0));

                assert_eq!(bytecode.chunk.code, $test_case)
            }
        };
    }

    macro_rules! test_broken_program {
        ($errors:ident, $name:ident, $text:literal, $test_case:expr) => {
            #[test]
            fn $name() {
                unsafe {
                    crate::debug::DEBUG_PRINT_CODE = true;
                    crate::debug::DEBUG_TRACE_EXECUTION = true;
                }
                let mut vm = VM::default();
                let mut scanner = Scanner::default();
                let mut parser = Parser::default();
                let compiler = Compiler::new(&mut scanner, &mut parser, &mut vm);

                println!("Compiling program:\n{}", $text);
                let mut err = compiler.compile($text).unwrap_err();
                let mut $errors = err.errors();

                $test_case
            }
        };
    }

    #[test]
    fn constants_in_variable_declaration() {
        let mut scanner = Scanner::default();
        let mut parser = Parser::default();
        let mut vm = VM::default();
        let compiler = Compiler::new(&mut scanner, &mut parser, &mut vm);
        let fun = compiler.compile("var x;").unwrap();

        let ptr = {
            let bytecode = &vm.get_function(fun);
            assert_eq!(1, bytecode.chunk.constants.len());
            if let Value::Object(ptr) = bytecode.chunk.constants.get(0).unwrap() {
                *ptr
            } else {
                panic!("expected the constant table to point to memory")
            }
        };

        if let Object::String(s) = vm.get_object(ptr) {
            assert_eq!(**s, "x");
        } else {
            panic!("expected memory to contian string variable name")
        }
    }

    test_program! {
        compile_variable_declaration,
        "var x;",
        vec![op::NIL, op::DEFINE_GLOBAL, 0, op::NIL, op::RETURN]
    }

    test_program! {
        compile_simple_integer_expression,
        "1;",
        vec![op::CONSTANT, 0, op::POP, op::NIL, op::RETURN]
    }

    test_program! {
        compile_print_expression,
        "print 1;",
        vec![op::CONSTANT, 0, op::PRINT, op::NIL, op::RETURN]
    }

    test_program! {
        compile_print_string,
        "print \"hello\";",
        vec![op::CONSTANT, 0, op::PRINT, op::NIL, op::RETURN]
    }

    test_program! {
        add_two_numbers,
        "1+1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 0, op::ADD, op::POP, op::NIL, op::RETURN]
    }

    test_program! {
        subtract_two_numbers,
        "1-1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 0, op::SUBTRACT, op::POP, op::NIL, op::RETURN]
    }

    test_program! {
        multiply_two_numbers,
        "1*1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 0, op::MULTIPLY, op::POP, op::NIL, op::RETURN]
    }

    test_program! {
        divide_two_numbers,
        "1/1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 0, op::DIVIDE, op::POP, op::NIL, op::RETURN]
    }

    test_program! {
        negate_a_number,
        "-1;",
        vec![op::CONSTANT, 0, op::NEGATE, op::POP, op::NIL, op::RETURN]
    }

    test_program! {
        compare_for_equality,
        "1 == 1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 0, op::EQUAL, op::POP, op::NIL, op::RETURN]
    }

    test_program! {
        greater_than_numbers,
        "2 > 1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 1, op::GREATER, op::POP, op::NIL, op::RETURN]
    }

    test_program! {
        greater_than_equal_numbers,
        "2 >= 1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 1, op::GREATER_EQUAL, op::POP, op::NIL, op::RETURN]
    }

    test_program! {
        less_than_numbers,
        "2 < 1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 1, op::GREATER_EQUAL, op::NOT, op::POP, op::NIL, op::RETURN]
    }

    test_program! {
        less_than_equal_numbers,
        "2 <= 1;",
        vec![op::CONSTANT, 0, op::CONSTANT, 1, op::GREATER, op::NOT, op::POP, op::NIL, op::RETURN]
    }

    test_program! {
        declare_variable,
        "var x;",
        vec![op::NIL, op::DEFINE_GLOBAL, 0, op::NIL, op::RETURN]
    }

    test_program! {
        define_global_variable,
        "var x = 10;",
        vec![op::CONSTANT, 1, op::DEFINE_GLOBAL, 0, op::NIL, op::RETURN]
    }

    test_program! {
        define_and_reference_global_variable,
        "
        var x = 10;
        x;
        ",
        vec![op::CONSTANT, 1, op::DEFINE_GLOBAL, 0, op::GET_GLOBAL, 0, op::POP, op::NIL, op::RETURN]
    }

    test_program! {
        simple_block_scope,
        "{ true; }",
        vec![op::CONSTANT, 0, op::POP, op::NIL, op::RETURN]
    }

    test_program! {
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
            op::NIL,
            op::RETURN
        ]
    }

    test_program! {
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
            op::NIL,
            op::RETURN
        ]
    }

    test_program! {
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
            6,
            op::CONSTANT,
            1,
            op::PRINT,
            op::JUMP,
            0,
            3,
            op::CONSTANT,
            2,
            op::PRINT,
            op::NIL,
            op::RETURN
        ]
    }

    test_program! {
        if_statement_no_else,
        "
        if (true) print 10;
        ",
        vec![
            op::CONSTANT,
            0,
            op::JUMP_IF_FALSE,
            0,
            3,
            op::CONSTANT,
            1,
            op::PRINT,
            op::NIL,
            op::RETURN
        ]
    }

    test_program! {
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
            3,
            op::CONSTANT,
            1,
            op::POP,
            op::NIL,
            op::RETURN
        ]
    }

    test_program! {
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
            6,
            op::CONSTANT,
            1,
            op::POP,
            op::LOOP,
            0,
            11,
            op::NIL,
            op::RETURN
        ]
    }

    test_program! {
        define_function_global,
        "
        fun f(a, b) {
            assert true;
        }
        ",
        vec![
            op::CLOSURE,
            1,
            0,
            op::DEFINE_GLOBAL,
            0,
            op::NIL,
            op::RETURN
        ]
    }

    test_program! {
        define_function,
        "
        {
            fun f(a, b) {
                assert true;
            }
        }
        ",
        vec![
            op::CLOSURE,
            0,
            0,
            op::POP,
            op::NIL,
            op::RETURN
        ]
    }

    test_program! {
        function_with_return,
        "
        fun f(a, b) {
            return a + b;
        }
        ",
        vec! [
            op::CLOSURE,
            1,
            0,
            op::DEFINE_GLOBAL,
            0,
            op::NIL,
            op::RETURN
        ]
    }

    test_program! {
        call_function,
        "
        fun f(a, b) {
            return a + b;
        }

        f(1, 2);
        ",
        vec! [
            op::CLOSURE,
            1,
            0,
            op::DEFINE_GLOBAL,
            0,
            op::GET_GLOBAL,
            0,
            op::CONSTANT,
            2,
            op::CONSTANT,
            3,
            op::CALL,
            2,
            op::POP,
            op::NIL,
            op::RETURN
        ]
    }

    test_program! {
        define_closure,
        "
        {
            var x = 10;
            fun f() {
                return x;
            }
        }
        ",
        vec![
            op::CONSTANT,
            0,
            op::CLOSURE,
            1,
            1,
            op::POP,
            op::POP,
            op::NIL,
            op::RETURN
        ]
    }

    test_broken_program! {
        errors,
        compile_error_incomplete_var_expression,
        "var;",
        {
            assert!(matches!(errors.pop().unwrap(), ParseError { .. }));
            assert!(matches!(errors.pop(), None));
        }
    }

    test_broken_program! {
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

    test_broken_program! {
        errors,
        compile_error_unterminated_string,
        "\"a",
        {
            assert_eq!(1, errors.len());
            assert!(matches!(errors.pop().unwrap(), ScanError { .. }));
        }
    }

    // Should terminate
    test_broken_program! {
        _errors,
        compile_error_should_terminate_at_eof,
        "0",
        ()
    }
}
