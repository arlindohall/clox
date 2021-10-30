use crate::object::MemoryEntry;
use crate::object::Object;
use crate::scanner::Scanner;
use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::scanner::TokenType::*;
use crate::value::Value;
use crate::vm::LoxError::ParseError;
use crate::vm::LoxErrorChain;
use crate::vm::Op;
use crate::vm::VM;

#[allow(dead_code)]
pub enum DebugOutput {
    None,
    Table,
    GraphViz,
}

pub(crate) static mut DEBUG_PRINT_CODE: DebugOutput = DebugOutput::None;
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

#[derive(Debug, Clone)]
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

struct ParseRule {
    prefix_rule: Option<ParseFn>,
    infix_rule: Option<ParseFn>,
    precedence: Prec,
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
    ///     Op::Nil
    ///     Op::DefineGlobal
    ///     1
    /// ```
    fn var_declaration(&mut self) {
        let name = self.parse_variable("Expect variable name.");

        if self.match_(Equal) {
            self.expression();
        } else {
            self.emit_byte(Op::Nil as u8);
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

        self.emit_bytes(Op::DefineGlobal as u8, name);
    }

    fn mark_initialized(&self) {
        todo!("mark the current token/variable as initialized")
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
        self.expression_with_precedence(Prec::Assignment);
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
        self.emit_byte(Op::Return as u8)
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

        let name = &self.parser.previous;

        let mut error = false;
        for local in &self.locals {
            if local.depth != UNINITIALIZED && local.depth < self.scope_depth {
                // There is a variable in the local stack that's not uninitialized but
                // is a level above the current set of locals (in terms of block scoping)
                break;
            }

            if self.identifiers_equal(name, &local.name) {
                error = true;
            }
        }
        self.add_local(&name);

        if error {
            self.error("Already a variable with this name in this scope.");
        }
    }

    fn identifiers_equal(&self, _first: &Token, _second: &Token) -> bool {
        todo!("compare two tokens")
    }

    fn add_local(&self, _name: &&Token) {
        todo!("add a local variable to the current scope")
    }

    fn identifier_constant(&mut self) -> u8 {
        let start = self.parser.previous.start;
        let end = self.parser.previous.length + self.parser.previous.start;
        let obj_pointer = self.copy_string(start, end);
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
        self.consume(Semicolon, "Expect ';' after expression.");
        self.emit_bytes(Op::Print as u8, Op::Pop as u8);
    }

    fn assert_statement(&mut self) {
        self.expression();
        self.consume(Semicolon, "Expect ';' after assertion statement.");
        self.emit_bytes(Op::Assert as u8, Op::Pop as u8);
    }

    fn for_statement(&self) {
        todo!("compile a for loop")
    }

    fn if_statement(&self) {
        todo!("compile an if statement")
    }

    fn return_statement(&self) {
        todo!("compile a return statement")
    }

    fn while_statement(&self) {
        todo!("compile a while statement")
    }

    fn epxression_statement(&mut self) {
        self.expression();
        self.consume(Semicolon, "Expect ';' after expression.");
        self.emit_byte(Op::Pop as u8);
    }

    fn block(&self) {
        todo!("compile a block")
    }

    fn begin_scope(&self) {
        todo!("begin a new scope")
    }

    fn end_scope(&self) {
        todo!("end a scope")
    }

    fn expression_with_precedence(&mut self, precedence: Prec) {
        let precedence: u8 = precedence.into();
        self.advance();

        let prefix_rule: Option<ParseFn> = self.previous_rule().prefix_rule;
        let can_assign = precedence <= Prec::Assignment.into();

        match prefix_rule {
            Some(rule) => rule(self, can_assign),
            None => self.error("Expect expression."),
        }

        while precedence <= self.current_rule().precedence.into() {
            self.advance();
            if let Some(infix_rule) = self.previous_rule().infix_rule {
                infix_rule(self, can_assign);
            }
        }

        if (can_assign) && self.match_(Equal) {
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
                precedence: Prec::And,
            },
            Assert => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            Bang => ParseRule {
                prefix_rule: Some(unary),
                infix_rule: None,
                precedence: Prec::None,
            },
            BangEqual => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: Prec::Equality,
            },
            Class => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            Comma => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            Dot => ParseRule {
                prefix_rule: None,
                infix_rule: Some(dot),
                precedence: Prec::Call,
            },
            Else => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            Eof => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            Equal => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            EqualEqual => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: Prec::Equality,
            },
            Error => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            False => ParseRule {
                prefix_rule: Some(literal),
                infix_rule: None,
                precedence: Prec::Primary,
            },
            For => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            Fun => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            Greater => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: Prec::Equality,
            },
            GreaterEqual => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: Prec::Equality,
            },
            Identifier => ParseRule {
                prefix_rule: Some(variable),
                infix_rule: None,
                precedence: Prec::None,
            },
            If => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            LeftBrace => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            LeftParen => ParseRule {
                prefix_rule: Some(grouping),
                infix_rule: None,
                precedence: Prec::None,
            },
            Less => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: Prec::Equality,
            },
            LessEqual => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: Prec::Equality,
            },
            Minus => ParseRule {
                prefix_rule: Some(unary),
                infix_rule: Some(binary),
                precedence: Prec::Term,
            },
            TokenNil => ParseRule {
                prefix_rule: Some(literal),
                infix_rule: None,
                precedence: Prec::None,
            },
            TokenNumber => ParseRule {
                prefix_rule: Some(number),
                infix_rule: None,
                precedence: Prec::Assignment,
            },
            Or => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: Prec::Or,
            },
            Plus => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: Prec::Term,
            },
            Print => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            Return => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            RightBrace => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            RightParen => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            Semicolon => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            Slash => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: Prec::Factor,
            },
            Star => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: Prec::Factor,
            },
            TokenString => ParseRule {
                prefix_rule: Some(string),
                infix_rule: None,
                precedence: Prec::Assignment,
            },
            Super => ParseRule {
                prefix_rule: Some(super_),
                infix_rule: None,
                precedence: Prec::None,
            },
            This => ParseRule {
                prefix_rule: Some(this_),
                infix_rule: None,
                precedence: Prec::None,
            },
            True => ParseRule {
                prefix_rule: Some(literal),
                infix_rule: None,
                precedence: Prec::Primary,
            },
            Var => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
            While => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: Prec::None,
            },
        }
    }

    fn copy_string(&mut self, start: usize, end: usize) -> MemoryEntry {
        let string = Object::String(self.scanner.copy_segment(start, end));
        self.vm.memory.allocate(string)
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        if self.function().chunk.constants.len() >= 256 {
            todo!("error handling for over-full constant table")
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
        todo!("recover after an error")
    }

    fn end_compiler(mut self) -> Result<MemoryEntry, LoxErrorChain> {
        self.emit_return();

        unsafe {
            match DEBUG_PRINT_CODE {
                DebugOutput::Table => self.function().disassemble_chunk(),
                DebugOutput::GraphViz => self.function().graph_output_chunk(),
                DebugOutput::None => (),
            }
        }

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
}

impl Prec {
    fn values() -> Vec<Prec> {
        vec![
            Prec::None,
            Prec::Assignment,
            Prec::Or,
            Prec::And,
            Prec::Equality,
            Prec::Comparison,
            Prec::Term,
            Prec::Factor,
            Prec::Unary,
            Prec::Call,
            Prec::Primary,
        ]
    }
}

impl From<Prec> for u8 {
    fn from(prec: Prec) -> Self {
        prec as u8
    }
}

impl From<u8> for Prec {
    fn from(num: u8) -> Self {
        for p in Prec::values() {
            let prec: u8 = p.clone().into();
            if prec == num {
                return p;
            }
        }

        panic!("Internal lox error: unable to match expression precedence.");
    }
}

/// Section: parse functions
fn number(this: &mut Compiler, _can_assign: bool) {
    let start = this.parser.previous.start;
    let end = start + this.parser.previous.length;
    let value = this.scanner.copy_segment(start, end).convert();

    let index = this.make_constant(Value::Number(value));
    this.emit_bytes(Op::Constant as u8, index);
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
    let mut uprec: u8 = rule.precedence.into();
    uprec += 1;
    let prec = uprec.into();

    this.expression_with_precedence(prec);

    match token {
        And => this.emit_byte(Op::And as u8),
        BangEqual => this.emit_bytes(Op::Equal as u8, Op::Not as u8),
        EqualEqual => this.emit_byte(Op::Equal as u8),
        Greater => this.emit_byte(Op::Greater as u8),
        GreaterEqual => this.emit_byte(Op::GreaterEqual as u8),
        Less => this.emit_bytes(Op::GreaterEqual as u8, Op::Not as u8),
        LessEqual => this.emit_bytes(Op::Greater as u8, Op::Not as u8),
        Minus => this.emit_byte(Op::Subtract as u8),
        Or => this.emit_byte(Op::Or as u8),
        Plus => this.emit_byte(Op::Add as u8),
        Slash => this.emit_byte(Op::Divide as u8),
        Star => this.emit_byte(Op::Multiply as u8),
        _ => this.error_at_current("Impossible binary operator. (this is an interpreter bug)"),
    }
}

fn unary(this: &mut Compiler, _can_assign: bool) {
    let token = this.parser.previous.type_.clone();
    let rule = this.get_rule(&token);
    let mut uprec: u8 = rule.precedence.into();
    uprec += 1;
    let prec = uprec.into();

    this.expression_with_precedence(prec);

    match token {
        Bang => this.emit_byte(Op::Not as u8),
        Minus => this.emit_byte(Op::Negate as u8),
        _ => this.error_at_current("Impossible unary operator. (this is an interpreter bug)"),
    }
}

fn grouping(this: &mut Compiler, _can_assign: bool) {
    this.expression();
    this.consume(RightParen, "Expected closing ')'.");
}

fn string(this: &mut Compiler, _can_assign: bool) {
    let start = this.parser.previous.start;
    let end = start + this.parser.previous.length;
    let value = this.scanner.copy_segment(start, end);
    let value = this.vm.memory.allocate(Object::String(value));

    let index = this.make_constant(Value::Object(value));
    this.emit_bytes(Op::Constant as u8, index);
}

fn literal(this: &mut Compiler, _can_assign: bool) {
    let token = &this.parser.previous.type_;
    let index = match token {
        True => this.make_constant(Value::Boolean(true)),
        False => this.make_constant(Value::Boolean(false)),
        TokenNil => this.make_constant(Value::Nil),
        _ => panic!("Internal error: cannot make literal (this is a bug)."),
    };

    this.emit_bytes(Op::Constant as u8, index);
}

fn this_(this: &mut Compiler, _can_assign: bool) {
    todo!("Compile a this expression for {:?}", this)
}

fn super_(this: &mut Compiler, _can_assign: bool) {
    todo!("Compile a super expression for {:?}", this)
}

fn variable(this: &mut Compiler, _can_assign: bool) {
    todo!("Compile a variable expression for {:?}", this)
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

impl Function {
    fn disassemble_chunk(&self) {
        eprintln!(
            "===== chunk {} =====",
            if !self.name.is_empty() {
                &self.name
            } else {
                "<script>"
            }
        );
        let mut i = 0;
        let mut line: usize = 0;

        while i < self.chunk.code.len() {
            let op = self.chunk.code.get(i).unwrap().into();
            let this_line = self.chunk.lines.get(i).unwrap();
            let line_part = if *this_line > line {
                line = *this_line;
                format!("{:<4}", this_line)
            } else {
                format!("{:<4}", "|")
            };

            i = match op {
                Op::Add => self.print_instruction(i, line_part, &op),
                Op::Assert => self.print_instruction(i, line_part, &op),
                Op::Constant => self.print_unary(i, line_part, &op),
                Op::DefineGlobal => self.print_unary(i, line_part, &op),
                Op::Nil => self.print_instruction(i, line_part, &op),
                Op::Pop => self.print_instruction(i, line_part, &op),
                Op::Print => self.print_instruction(i, line_part, &op),
                Op::Return => self.print_instruction(i, line_part, &op),
                Op::Negate => self.print_instruction(i, line_part, &op),
                Op::Not => self.print_instruction(i, line_part, &op),
                Op::Subtract => self.print_instruction(i, line_part, &op),
                Op::And => self.print_instruction(i, line_part, &op),
                Op::Divide => self.print_instruction(i, line_part, &op),
                Op::Equal => self.print_instruction(i, line_part, &op),
                Op::Greater => self.print_instruction(i, line_part, &op),
                Op::GreaterEqual => self.print_instruction(i, line_part, &op),
                Op::Or => self.print_instruction(i, line_part, &op),
                Op::Multiply => self.print_instruction(i, line_part, &op),
            }
        }
    }

    fn graph_output_chunk(&self) {
        eprintln!("digraph chunk {{");
        let mut i = 0;

        while i < self.chunk.code.len() - 1 {
            let op = self.chunk.code.get(i).unwrap().into();

            i = match op {
                Op::Add => self.graph_instruction(i, &op),
                Op::Assert => self.graph_instruction(i, &op),
                Op::Constant => self.graph_unary(i, &op),
                Op::DefineGlobal => self.graph_unary(i, &op),
                Op::Nil => self.graph_instruction(i, &op),
                Op::Pop => self.graph_instruction(i, &op),
                Op::Print => self.graph_instruction(i, &op),
                Op::Return => self.graph_instruction(i, &op),
                Op::Negate => self.graph_instruction(i, &op),
                Op::Not => self.graph_instruction(i, &op),
                Op::Subtract => self.graph_instruction(i, &op),
                Op::And => self.graph_instruction(i, &op),
                Op::Divide => self.graph_instruction(i, &op),
                Op::Equal => self.graph_instruction(i, &op),
                Op::Greater => self.graph_instruction(i, &op),
                Op::GreaterEqual => self.graph_instruction(i, &op),
                Op::Or => self.graph_instruction(i, &op),
                Op::Multiply => self.graph_instruction(i, &op),
            }
        }

        eprintln!("}}")
    }

    fn print_unary(&self, i: usize, line_part: String, op: &Op) -> usize {
        // todo: graph the values instead of the pointer
        let val = self.chunk.code.get(i + 1).unwrap();
        let op = format!("{:?}", op);

        eprintln!(
            "{:04} {}{:16}{:4} '{}'",
            i,
            line_part,
            op,
            val,
            self.chunk.constants.get(*val as usize).unwrap()
        );
        i + 2
    }

    fn print_instruction(&self, i: usize, line_part: String, op: &Op) -> usize {
        eprintln!("{:04} {}{:?}", i, line_part, op);
        i + 1
    }

    fn graph_unary(&self, i: usize, op: &Op) -> usize {
        // todo: graph the constant itself, not the pointer
        let c = self.chunk.code.get(i + 1).unwrap();
        let next: Op = self.chunk.code.get(i + 2).unwrap().into();

        let c = self.chunk.constants.get(*c as usize).unwrap();

        eprintln!("\"{}: {:?}\" -> \"{}: {}\";", i, op, i + 1, c);
        eprintln!("\"{}: {:?}\" -> \"{}: {:?}\";", i, op, i + 2, next);

        i + 2
    }

    fn graph_instruction(&self, i: usize, op: &Op) -> usize {
        let next: Op = self.chunk.code.get(i + 1).unwrap().into();
        eprintln!("\"{}: {:?}\" -> \"{}: {:?}\";", i, op, i + 1, next);

        i + 1
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::vm::LoxError::ScanError;

    fn compile_expression(expr: &str) -> (MemoryEntry, VM) {
        let mut vm = VM::default();
        let compiler = Compiler::new(&mut vm);

        let function = match compiler.compile(expr) {
            Ok(f) => f,
            Err(e) => {
                println!("Error in test: {}", e);
                panic!()
            }
        };

        vm.memory
            .retrieve(&function)
            .as_function()
            .disassemble_chunk();
        (function, vm)
    }

    fn compile_broken(expr: &str) -> (LoxErrorChain, VM) {
        let mut vm = VM::default();
        let compiler = Compiler::new(&mut vm);

        (compiler.compile(expr).unwrap_err(), vm)
    }

    #[test]
    fn compile_variable_declaration() {
        let (bytecode, vm) = compile_expression("var x;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![Op::Nil as u8, Op::DefineGlobal as u8, 0, Op::Return as u8]
        );

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

    #[test]
    fn compile_simple_integer_expression() {
        let (bytecode, vm) = compile_expression("1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![Op::Constant as u8, 0, Op::Pop as u8, Op::Return as u8]
        );
    }

    #[test]
    fn compile_print_expression() {
        let (bytecode, vm) = compile_expression("print 1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                0,
                Op::Print as u8,
                Op::Pop as u8,
                Op::Return as u8
            ]
        );
    }

    #[test]
    fn compile_print_string() {
        let (bytecode, vm) = compile_expression("print \"hello\";");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                0,
                Op::Print as u8,
                Op::Pop as u8,
                Op::Return as u8
            ]
        );
    }

    #[test]
    fn add_two_numbers() {
        let (bytecode, vm) = compile_expression("1+1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                0,
                Op::Constant as u8,
                1,
                Op::Add as u8,
                Op::Pop as u8,
                Op::Return as u8,
            ]
        );
    }

    #[test]
    fn subtract_two_numbers() {
        let (bytecode, vm) = compile_expression("1-1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                0,
                Op::Constant as u8,
                1,
                Op::Subtract as u8,
                Op::Pop as u8,
                Op::Return as u8,
            ]
        );
    }

    #[test]
    fn multiply_two_numbers() {
        let (bytecode, vm) = compile_expression("1*1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                0,
                Op::Constant as u8,
                1,
                Op::Multiply as u8,
                Op::Pop as u8,
                Op::Return as u8,
            ]
        );
    }

    #[test]
    fn divide_two_numbers() {
        let (bytecode, vm) = compile_expression("1/1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                0,
                Op::Constant as u8,
                1,
                Op::Divide as u8,
                Op::Pop as u8,
                Op::Return as u8,
            ]
        );
    }

    #[test]
    fn negate_a_number() {
        let (bytecode, vm) = compile_expression("-1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                0,
                Op::Negate as u8,
                Op::Pop as u8,
                Op::Return as u8,
            ]
        );
    }

    #[test]
    fn compare_for_equality() {
        let (bytecode, vm) = compile_expression("1 == 1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                0,
                Op::Constant as u8,
                1,
                Op::Equal as u8,
                Op::Pop as u8,
                Op::Return as u8,
            ]
        );
    }

    #[test]
    fn greater_than_numbers() {
        let (bytecode, vm) = compile_expression("2 > 1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                0,
                Op::Constant as u8,
                1,
                Op::Greater as u8,
                Op::Pop as u8,
                Op::Return as u8,
            ]
        );
    }

    #[test]
    fn greater_than_equal_numbers() {
        let (bytecode, vm) = compile_expression("2 >= 1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                0,
                Op::Constant as u8,
                1,
                Op::GreaterEqual as u8,
                Op::Pop as u8,
                Op::Return as u8,
            ]
        );
    }

    #[test]
    fn less_than_numbers() {
        let (bytecode, vm) = compile_expression("2 < 1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                0,
                Op::Constant as u8,
                1,
                Op::GreaterEqual as u8,
                Op::Not as u8,
                Op::Pop as u8,
                Op::Return as u8,
            ]
        );
    }

    #[test]
    fn less_than_equal_numbers() {
        let (bytecode, vm) = compile_expression("2 <= 1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                0,
                Op::Constant as u8,
                1,
                Op::Greater as u8,
                Op::Not as u8,
                Op::Pop as u8,
                Op::Return as u8,
            ]
        );
    }

    #[test]
    fn declare_variable() {
        let (bytecode, vm) = compile_expression("var x;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![Op::Nil as u8, Op::DefineGlobal as u8, 0, Op::Return as u8,]
        )
    }

    #[test]
    fn define_global_variable() {
        let (bytecode, vm) = compile_expression("var x = 10;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                1,
                Op::DefineGlobal as u8,
                0,
                Op::Return as u8,
            ]
        )
    }

    #[test]
    fn compile_error_incomplete_var_expression() {
        let (mut err, _vm) = compile_broken("var;");
        let mut errors = err.errors();

        assert!(matches!(errors.pop().unwrap(), ParseError { .. }));
        assert!(matches!(errors.pop(), None));
    }

    #[test]
    fn compile_error_unexpected_end_of_expr() {
        let (mut err, _vm) = compile_broken(";");
        let mut errors = err.errors();

        assert_eq!(2, errors.len());

        let expr_err = errors.pop().unwrap();
        let semi_err = errors.pop().unwrap();

        assert!(matches!(&expr_err, ParseError { .. }));
        assert!(matches!(&semi_err, ParseError { .. }));
    }

    #[test]
    fn compile_error_unterminated_string() {
        let (mut err, _vm) = compile_broken("\"a");
        let mut errors = err.errors();

        assert_eq!(1, errors.len());
        assert!(matches!(errors.pop().unwrap(), ScanError { .. }));
    }
}
