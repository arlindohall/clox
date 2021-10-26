use crate::object::MemoryEntry;
use crate::object::Object::*;
use crate::scanner::Scanner;
use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::scanner::TokenType::*;
use crate::value::{Value, Value::*};
use crate::vm::LoxError::*;
use crate::vm::LoxErrorChain;
use crate::vm::LoxErrorSpec;
use crate::vm::Op;
use crate::vm::Op::*;
use crate::vm::VM;

#[allow(dead_code)]
pub(crate) enum DebugOutput {
    None,
    Table,
    GraphViz,
}

static DEBUG_PRINT_CODE: DebugOutput = DebugOutput::GraphViz;
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
    pub(crate) chunk: Vec<u8>,
    constants: Vec<Value>,
    pub(crate) arity: usize,
}

#[derive(Debug, Clone)]
enum Precedence {
    PrecNone,
    PrecAssignment,
    _PrecOr,
    _PrecAnd,
    _PrecEquality,
    _PrecComparison,
    PrecTerm,
    _PrecFactor,
    _PrecUnary,
    _PrecCall,
    _PrecPrimary,
}

struct ParseRule {
    prefix_rule: Option<ParseFn>,
    infix_rule: Option<ParseFn>,
    precedence: Precedence,
}

type ParseFn = fn(&mut Compiler, bool);

use bigdecimal::BigDecimal;
use bigdecimal::Num;
use bigdecimal::ToPrimitive;
use Precedence::*;

impl<'a> Compiler<'a> {
    /// Create a new compiler with empty source and no errors.
    ///
    /// This method also initializes the scanner and parser, and
    /// is fine to use any time we need a new [Compiler]
    pub fn new(vm: &mut VM) -> Compiler {
        let entry_point = Function {
            arity: 0,
            chunk: Vec::new(),
            constants: Vec::new(),
        };
        let function = vm.memory.allocate(ObjFunction(Box::new(entry_point)));
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
    ///     OpNil
    ///     OpDefineGlobal
    ///     1
    /// ```
    fn var_declaration(&mut self) {
        let name = self.parse_variable("Expect variable name.");

        if self.match_(Equal) {
            self.expression();
        } else {
            self.emit_byte(OpNil as u8);
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

        self.emit_bytes(OpDefineGlobal as u8, name);
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
        self.expression_with_precedence(PrecAssignment);
    }

    fn emit_bytes(&mut self, op1: u8, op2: u8) {
        self.emit_byte(op1);
        self.emit_byte(op2);
    }

    fn emit_byte(&mut self, op: u8) {
        self.function().chunk.push(op)
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpReturn as u8)
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
        self.make_constant(Object(obj_pointer))
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
        self.emit_bytes(OpPrint as u8, OpPop as u8);
    }

    fn assert_statement(&self) {
        todo!("compile a single assertion")
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
        self.emit_byte(OpPop as u8);
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

    fn expression_with_precedence(&mut self, precedence: Precedence) {
        let precedence: u8 = precedence.into();
        self.advance();

        let prefix_rule: Option<ParseFn> = self.previous_rule().prefix_rule;
        let can_assign = precedence <= PrecAssignment.into();

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
            And => todo!(),
            Assert => todo!(),
            Bang => todo!(),
            BangEqual => todo!(),
            Class => todo!(),
            Comma => todo!(),
            Dot => todo!(),
            Else => todo!(),
            Eof => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: PrecNone,
            },
            Equal => todo!(),
            EqualEqual => todo!(),
            Error => todo!(),
            False => todo!(),
            For => todo!(),
            Fun => todo!(),
            Greater => todo!(),
            GreaterEqual => todo!(),
            Identifier => todo!(),
            If => todo!(),
            LeftBrace => todo!(),
            LeftParen => todo!(),
            Less => todo!(),
            LessEqual => todo!(),
            Minus => ParseRule {
                prefix_rule: Some(unary),
                infix_rule: Some(binary),
                precedence: PrecTerm,
            },
            Nil => todo!(),
            TokenNumber => ParseRule {
                prefix_rule: Some(number),
                infix_rule: None,
                precedence: PrecAssignment,
            },
            Or => todo!(),
            Plus => ParseRule {
                prefix_rule: None,
                infix_rule: Some(binary),
                precedence: PrecTerm,
            },
            Print => todo!(),
            Return => todo!(),
            RightBrace => todo!(),
            RightParen => todo!(),
            Semicolon => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: PrecNone,
            },
            Slash => todo!(),
            Star => todo!(),
            TokenString => ParseRule {
                prefix_rule: Some(string),
                infix_rule: None,
                precedence: PrecAssignment,
            },
            Super => todo!(),
            This => todo!(),
            True => todo!(),
            Var => todo!(),
            While => todo!(),
        }
    }

    fn copy_string(&mut self, start: usize, end: usize) -> MemoryEntry {
        let string = ObjString(self.scanner.copy_segment(start, end));
        self.vm.memory.allocate(string)
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        if self.function().constants.len() >= 256 {
            todo!("error handling for over-full constant table")
        } else {
            self.function().constants.push(value);
            (self.function().constants.len() - 1) as u8
        }
    }

    fn error_at_current(&mut self, message: &str) {
        let message = message.to_string();
        self.error_chain.register(ParseError(LoxErrorSpec {
            line: self.parser.current.line,
            message,
        }))
    }

    fn error(&mut self, message: &str) {
        let message = message.to_string();
        self.error_chain.register(ParseError(LoxErrorSpec {
            line: self.parser.previous.line,
            message,
        }))
    }

    fn synchronize(&mut self) {
        todo!("recover after an error")
    }

    fn end_compiler(mut self) -> Result<MemoryEntry, LoxErrorChain> {
        self.emit_return();

        match DEBUG_PRINT_CODE {
            DebugOutput::Table => self.function().disassemble_chunk(),
            DebugOutput::GraphViz => self.function().graph_output_chunk(),
            DebugOutput::None => (),
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

impl Precedence {
    fn values() -> Vec<Precedence> {
        vec![
            PrecNone,
            PrecAssignment,
            _PrecOr,
            _PrecAnd,
            _PrecEquality,
            _PrecComparison,
            PrecTerm,
            _PrecFactor,
            _PrecUnary,
            _PrecCall,
            _PrecPrimary,
        ]
    }
}

impl From<Precedence> for u8 {
    fn from(prec: Precedence) -> Self {
        match prec {
            PrecNone => 0,
            PrecAssignment => 1,
            _PrecOr => 2,
            _PrecAnd => 3,
            _PrecEquality => 4,
            _PrecComparison => 5,
            PrecTerm => 6,
            _PrecFactor => 7,
            _PrecUnary => 8,
            _PrecCall => 9,
            _PrecPrimary => 10,
        }
    }
}

impl From<u8> for Precedence {
    fn from(num: u8) -> Self {
        for p in Precedence::values() {
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
    this.emit_bytes(OpConstant as u8, index);
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
        And => todo!(),
        BangEqual => todo!(),
        EqualEqual => todo!(),
        Greater => todo!(),
        GreaterEqual => todo!(),
        Less => todo!(),
        LessEqual => todo!(),
        Minus => this.emit_byte(OpSubtract as u8),
        Or => todo!(),
        Plus => this.emit_byte(OpAdd as u8),
        Slash => todo!(),
        Star => todo!(),
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
        Bang => this.emit_byte(OpNot as u8),
        Minus => this.emit_byte(OpNegate as u8),
        _ => this.error_at_current("Impossible unary operator. (this is an interpreter bug)"),
    }
}

fn string(this: &mut Compiler, _can_assign: bool) {
    let start = this.parser.previous.start;
    let end = start + this.parser.previous.length;
    let value = this.scanner.copy_segment(start, end);
    let value = this.vm.memory.allocate(ObjString(value));

    let index = this.make_constant(Value::Object(value));
    this.emit_bytes(OpConstant as u8, index);
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
        eprintln!("digraph chunk {{");
        for (instruction, op) in self.chunk.iter().enumerate() {
            let op = op.into();
            self.print_instruction(instruction as u32, &op);
        }
        eprintln!("}}");
        todo!("output memory and code as graphviz")
    }

    fn graph_output_chunk(&self) {
        eprintln!("digraph chunk {{");
        let mut i = 0;

        while i < self.chunk.len() - 1 {
            let op = self.chunk.get(i).unwrap().into();

            match op {
                OpDefineGlobal => self.graph_instruction(i, &op),
                OpConstant => {
                    self.graph_binary(i, &op);
                    i += 1;
                }
                OpPop => self.graph_instruction(i, &op),
                OpPrint => self.graph_instruction(i, &op),
                OpNil => self.graph_instruction(i, &op),
                OpReturn => self.graph_instruction(i, &op),
                OpAdd => todo!("visualize binary add operation"),
                OpNegate => todo!("visualize binary negate operation"),
                OpNot => {
                    self.graph_binary(i, &op);
                    i += 1;
                }
                OpSubtract => todo!(),
            }

            i += 1;
        }
        eprintln!("}}");
    }

    fn print_instruction(&self, instruction: u32, op: &Op) {
        eprintln!("{}{:?};", instruction, op);
    }

    fn graph_binary(&self, i: usize, op: &Op) {
        // todo: graph the constant itself, not the pointer
        let c = self.chunk.get(i + 1).unwrap();
        let next: Op = self.chunk.get(i + 2).unwrap().into();
        eprintln!("\"{}: {:?}\" -> \"{}: {}\";", i, op, i + 1, c);
        eprintln!("\"{}: {:?}\" -> \"{}: {:?}\";", i, op, i + 2, next);
    }

    fn graph_instruction(&self, i: usize, op: &Op) {
        let next: Op = self.chunk.get(i + 1).unwrap().into();
        eprintln!("\"{}: {:?}\" -> \"{}: {:?}\";", i, op, i + 1, next);
    }
}

#[cfg(test)]
mod test {

    use super::*;

    fn compile_expression(expr: &str) -> (MemoryEntry, VM) {
        let mut vm = VM::default();
        let compiler = Compiler::new(&mut vm);
        let function = compiler.compile(expr).unwrap();

        (function, vm)
    }

    fn compile_broken(expr: &str) -> (LoxErrorChain, VM) {
        let mut vm = VM::default();
        let compiler = Compiler::new(&mut vm);

        (compiler.compile(expr).unwrap_err(), vm)
    }

    #[test]
    fn test_compile_variable_declaration() {
        let (bytecode, vm) = compile_expression("var x;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk,
            vec![OpNil as u8, OpDefineGlobal as u8, 0, OpReturn as u8]
        );

        assert_eq!(1, bytecode.constants.len());

        if let Object(ptr) = bytecode.constants.get(0).unwrap() {
            if let ObjString(s) = vm.memory.retrieve(ptr) {
                assert_eq!(**s, "x");
            } else {
                panic!("expected memory to contian string variable name")
            }
        } else {
            panic!("expected the constant table to point to memory")
        }
    }

    #[test]
    fn test_compile_simple_integer_expression() {
        let (bytecode, vm) = compile_expression("1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk,
            vec![OpConstant as u8, 0, OpPop as u8, OpReturn as u8]
        );
    }

    #[test]
    fn test_compile_print_expression() {
        let (bytecode, vm) = compile_expression("print 1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk,
            vec![
                OpConstant as u8,
                0,
                OpPrint as u8,
                OpPop as u8,
                OpReturn as u8
            ]
        );
    }

    #[test]
    fn test_compile_print_string() {
        let (bytecode, vm) = compile_expression("print \"hello\";");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk,
            vec![
                OpConstant as u8,
                0,
                OpPrint as u8,
                OpPop as u8,
                OpReturn as u8
            ]
        );
    }

    #[test]
    fn test_add_two_numbers() {
        let (bytecode, vm) = compile_expression("1+1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk,
            vec![
                OpConstant as u8,
                0,
                OpConstant as u8,
                1,
                OpAdd as u8,
                OpPop as u8,
                OpReturn as u8,
            ]
        );
    }

    #[test]
    fn test_subtract_two_numbers() {
        let (bytecode, vm) = compile_expression("1-1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk,
            vec![
                OpConstant as u8,
                0,
                OpConstant as u8,
                1,
                OpSubtract as u8,
                OpPop as u8,
                OpReturn as u8,
            ]
        );
    }

    #[test]
    fn test_negate_a_number() {
        let (bytecode, vm) = compile_expression("-1;");
        let bytecode = vm.memory.retrieve(&bytecode).as_function();

        assert_eq!(
            bytecode.chunk,
            vec![
                OpConstant as u8,
                0,
                OpNegate as u8,
                OpPop as u8,
                OpReturn as u8,
            ]
        );
    }

    #[test]
    fn test_compile_error_unexpected_end_of_expr() {
        let (mut err, _vm) = compile_broken(";");
        let mut errors = err.errors();

        assert_eq!(2, errors.len());

        let expr_err = errors.pop().unwrap();
        let semi_err = errors.pop().unwrap();

        assert!(match &expr_err {
            ParseError(_) => true,
            _ => false,
        });
        assert!(match &semi_err {
            ParseError(_) => true,
            _ => false,
        });
    }

    #[test]
    fn test_compile_error_unterminated_string() {
        let (mut err, _vm) = compile_broken("\"a");
        let mut errors = err.errors();

        assert_eq!(1, errors.len());
        assert!(match errors.pop().unwrap() {
            ScanError(_) => true,
            _ => false,
        });
    }
}
