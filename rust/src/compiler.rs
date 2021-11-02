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

const PRECS: [Prec; 11] = [
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
];

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
            self.emit_byte(Op::Pop as u8);
            self.locals.pop();
        }
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

    fn copy_string(&mut self) -> MemoryEntry {
        let token = &self.parser.previous;
        let string = Object::String(self.scanner.copy_segment(token));
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

    fn named_variable(&mut self, can_assign: bool) {
        // Local variable
        let arg: isize = self.resolve_local();
        if arg > 0 && self.match_(TokenType::Equal) {
            // UNINITIALIZED (-1) is the only negative value
            self.expression();
            return self.emit_bytes(Op::SetLocal as u8, arg as u8);
        } else if arg > 0 {
            return self.emit_bytes(Op::GetLocal as u8, arg as u8);
        }

        // Global variable
        let arg = self.identifier_constant();

        if can_assign && self.match_(TokenType::Equal) {
            self.expression();
            self.emit_bytes(Op::SetGlobal as u8, arg)
        } else {
            self.emit_bytes(Op::GetGlobal as u8, arg)
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

impl From<Prec> for u8 {
    fn from(prec: Prec) -> Self {
        prec as u8
    }
}

impl From<u8> for Prec {
    fn from(num: u8) -> Self {
        PRECS[num as usize].clone()
    }
}

/// Section: parse functions
fn number(this: &mut Compiler, _can_assign: bool) {
    let value = this.scanner.copy_segment(&this.parser.previous).convert();

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
    let value = this.scanner.copy_segment(&this.parser.previous);
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

            let action = match op {
                Op::Add => Self::print_instruction,
                Op::Assert => Self::print_instruction,
                Op::Constant => Self::print_constant,
                Op::DefineGlobal => Self::print_constant,
                Op::GetGlobal => Self::print_constant,
                Op::SetGlobal => Self::print_constant,
                Op::GetLocal => Self::print_local,
                Op::SetLocal => Self::print_local,
                Op::Nil => Self::print_instruction,
                Op::Pop => Self::print_instruction,
                Op::Print => Self::print_instruction,
                Op::Return => Self::print_instruction,
                Op::Negate => Self::print_instruction,
                Op::Not => Self::print_instruction,
                Op::Subtract => Self::print_instruction,
                Op::And => Self::print_instruction,
                Op::Divide => Self::print_instruction,
                Op::Equal => Self::print_instruction,
                Op::Greater => Self::print_instruction,
                Op::GreaterEqual => Self::print_instruction,
                Op::Or => Self::print_instruction,
                Op::Multiply => Self::print_instruction,
            };

            i = action(self, i, line_part, &op);
        }
    }

    fn graph_output_chunk(&self) {
        eprintln!("digraph chunk {{");
        let mut i = 0;

        while i < self.chunk.code.len() - 1 {
            let op = self.chunk.code.get(i).unwrap().into();

            let action = match op {
                Op::Add => Self::graph_instruction,
                Op::Assert => Self::graph_instruction,
                Op::Constant => Self::graph_constant,
                Op::DefineGlobal => Self::graph_constant,
                Op::GetGlobal => Self::graph_constant,
                Op::SetGlobal => Self::graph_constant,
                Op::GetLocal => Self::graph_local,
                Op::SetLocal => Self::graph_local,
                Op::Nil => Self::graph_instruction,
                Op::Pop => Self::graph_instruction,
                Op::Print => Self::graph_instruction,
                Op::Return => Self::graph_instruction,
                Op::Negate => Self::graph_instruction,
                Op::Not => Self::graph_instruction,
                Op::Subtract => Self::graph_instruction,
                Op::And => Self::graph_instruction,
                Op::Divide => Self::graph_instruction,
                Op::Equal => Self::graph_instruction,
                Op::Greater => Self::graph_instruction,
                Op::GreaterEqual => Self::graph_instruction,
                Op::Or => Self::graph_instruction,
                Op::Multiply => Self::graph_instruction,
            };

            i = action(self, i, &op);
        }

        eprintln!("}}")
    }

    fn print_local(&self, i: usize, line_part: String, op: &Op) -> usize {
        // todo: graph the values instead of the pointer
        let val = self.chunk.code.get(i + 1).unwrap();
        let op = format!("{:?}", op);

        eprintln!("{:04} {}{:16}{:4}", i, line_part, op, val,);
        i + 2
    }

    fn print_constant(&self, i: usize, line_part: String, op: &Op) -> usize {
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

    fn graph_local(&self, i: usize, op: &Op) -> usize {
        // todo: graph the constant itself, not the pointer
        let c = self.chunk.code.get(i + 1).unwrap();
        let next: Op = self.chunk.code.get(i + 2).unwrap().into();

        eprintln!("\"{}: {:?}\" -> \"{}: {}\";", i, op, i + 1, c);
        eprintln!("\"{}: {:?}\" -> \"{}: {:?}\";", i, op, i + 2, next);

        i + 2
    }

    fn graph_constant(&self, i: usize, op: &Op) -> usize {
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

    fn function(vm: &VM) -> &Function {
        vm.memory.retrieve(&crate::object::mem(0)).as_function()
    }

    fn compile_expression(expr: &str) -> VM {
        let mut vm = VM::default();
        let compiler = Compiler::new(&mut vm);

        match compiler.compile(expr) {
            Ok(_) => {
                function(&vm).disassemble_chunk();
                vm
            }
            Err(e) => {
                println!("Error in test: {}", e);
                panic!("Failing test: expected code to compile.")
            }
        }
    }

    fn compile_broken(expr: &str) -> (LoxErrorChain, VM) {
        let mut vm = VM::default();
        let compiler = Compiler::new(&mut vm);

        (compiler.compile(expr).unwrap_err(), vm)
    }

    #[test]
    fn compile_variable_declaration() {
        let vm = compile_expression("var x;");
        let bytecode = function(&vm);

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
        let vm = compile_expression("1;");
        let bytecode = function(&vm);

        assert_eq!(
            bytecode.chunk.code,
            vec![Op::Constant as u8, 0, Op::Pop as u8, Op::Return as u8]
        );
    }

    #[test]
    fn compile_print_expression() {
        let vm = compile_expression("print 1;");
        let bytecode = function(&vm);

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
        let vm = compile_expression("print \"hello\";");
        let bytecode = function(&vm);

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
        let vm = compile_expression("1+1;");
        let bytecode = function(&vm);

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
        let vm = compile_expression("1-1;");
        let bytecode = function(&vm);

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
        let vm = compile_expression("1*1;");
        let bytecode = function(&vm);

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
        let vm = compile_expression("1/1;");
        let bytecode = function(&vm);

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
        let vm = compile_expression("-1;");
        let bytecode = function(&vm);

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
        let vm = compile_expression("1 == 1;");
        let bytecode = function(&vm);

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
        let vm = compile_expression("2 > 1;");
        let bytecode = function(&vm);

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
        let vm = compile_expression("2 >= 1;");
        let bytecode = function(&vm);

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
        let vm = compile_expression("2 < 1;");
        let bytecode = function(&vm);

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
        let vm = compile_expression("2 <= 1;");
        let bytecode = function(&vm);

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
        let vm = compile_expression("var x;");
        let bytecode = function(&vm);

        assert_eq!(
            bytecode.chunk.code,
            vec![Op::Nil as u8, Op::DefineGlobal as u8, 0, Op::Return as u8,]
        )
    }

    #[test]
    fn define_global_variable() {
        let vm = compile_expression("var x = 10;");
        let bytecode = function(&vm);

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
    fn define_and_reference_global_variable() {
        let vm = compile_expression(
            "var x = 10;
            x;",
        );
        let bytecode = function(&vm);

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                1,
                Op::DefineGlobal as u8,
                0,
                Op::GetGlobal as u8,
                2,
                Op::Pop as u8,
                Op::Return as u8,
            ]
        )
    }

    #[test]
    fn simple_block_scope() {
        let vm = compile_expression(
            "
            {
                true;
            }
        ",
        );
        let bytecode = function(&vm);

        assert_eq!(
            bytecode.chunk.code,
            vec![Op::Constant as u8, 0, Op::Pop as u8, Op::Return as u8,]
        )
    }

    #[test]
    fn block_scope_locals() {
        let vm = compile_expression(
            "
            var x = 10;
            {
                var x = 20;
            }
            {
                var x = 30;
            }
        ",
        );
        let bytecode = function(&vm);

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Constant as u8,
                1,
                Op::DefineGlobal as u8,
                0,
                Op::Constant as u8,
                2,
                Op::Pop as u8,
                Op::Constant as u8,
                3,
                Op::Pop as u8,
                Op::Return as u8,
            ]
        )
    }

    #[test]
    fn block_scope_locals_get_and_set() {
        let vm = compile_expression(
            "
            {
                var x;
                x = 10;
                x;
            }
        ",
        );
        let bytecode = function(&vm);

        assert_eq!(
            bytecode.chunk.code,
            vec![
                Op::Nil as u8,
                Op::Constant as u8,
                0,
                Op::SetLocal as u8,
                1,
                Op::Pop as u8,
                Op::GetLocal as u8,
                1,
                Op::Pop as u8,
                Op::Pop as u8,
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
