use std::error::Error;

use crate::object::Object::*;
use crate::scanner::Scanner;
use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::scanner::TokenType::*;
use crate::value::{Value, Value::*};
use crate::vm::Op::*;
use crate::vm::VM;

static DEBUG_PRINT_CODE: bool = false;
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

    function: Function,
    locals: Vec<Local>,

    scope_depth: isize,
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
    had_error: bool,

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
    chunk: Vec<u8>,
    constants: Vec<Value>,
}

#[derive(Debug)]
enum Precedence {
    PrecNone,
    PrecAssignment,
    _PrecOr,
    _PrecAnd,
    _PrecEquality,
    _PrecComparison,
    _PrecTerm,
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

use Precedence::*;
use bigdecimal::BigDecimal;
use bigdecimal::Num;
use bigdecimal::ToPrimitive;

impl<'a> Compiler<'a> {
    /// Create a new compiler with empty source and no errors.
    ///
    /// This method also initializes the scanner and parser, and
    /// is fine to use any time we need a new [Compiler]
    pub fn new(vm: &mut VM) -> Compiler {
        Compiler {
            vm,
            ancestors: Vec::new(),
            scanner: Scanner::default(),
            parser: Parser {
                had_error: false,
                current: Token::default(),
                previous: Token::default(),
                panic_mode: false,
            },
            scope_depth: 0,
            locals: Vec::new(),
            function: Function {
                chunk: Vec::new(),
                constants: Vec::new(),
            },
        }
    }

    /// Compile the statement given and report errors back to the VM.
    ///
    /// The statement passed in can be a group of statements separated
    /// by a ';' character, as specified in Lox grammar.
    pub fn compile(mut self, statement: &str) -> Result<Function, Box<dyn Error>> {
        self.scanner.take_str(statement);
        self.parser.had_error = false;

        self.advance();

        while !self.match_(TokenEof) {
            self.declaration();
        }

        let function = self.end_compiler();

        Ok(function)
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

            if self.parser.current.type_ != TokenError {
                break;
            }

            let start = self.parser.current.start;
            let end = self.parser.current.start + self.parser.current.length;
            self.error_at_current(&self.scanner.copy_segment(start, end));
        }
    }

    /// If the current (next to be compiled) token matches, then advance
    ///
    /// Return whether the token was matched, and the skip-ahead
    /// behavior is sort of hidden from the caller. But that lets the
    /// caller do cool things like the following:
    ///
    /// ```ignore
    /// let compiler = Compiler::new(&vm);
    /// if compiler.match_(TokenEof) {
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
        if self.match_(TokenClass) {
            self.class_declaration();
        } else if self.match_(TokenFun) {
            self.fun_declaration();
        } else if self.match_(TokenVar) {
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

        if self.match_(TokenEqual) {
            self.expression();
        } else {
            self.emit_byte(OpNil as u8);
        }
        self.consume(TokenSemicolon, "Expect ';' after variable declaration.");

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
        self.function.chunk.push(op)
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpReturn as u8)
    }

    fn parse_variable(&mut self, message: &str) -> u8 {
        self.consume(TokenIdentifier, message);

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

    fn add_local(&self, _name: &&Token) -> () {
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
        if self.match_(TokenPrint) {
            self.print_statement();
        } else if self.match_(TokenAssert) {
            self.assert_statement();
        } else if self.match_(TokenFor) {
            self.for_statement();
        } else if self.match_(TokenIf) {
            self.if_statement();
        } else if self.match_(TokenReturn) {
            self.return_statement();
        } else if self.match_(TokenWhile) {
            self.while_statement();
        } else if self.match_(TokenLeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.epxression_statement();
        }
    }

    fn print_statement(&self) {
        todo!("compile a single print statement")
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
        self.consume(TokenSemicolon, "Expect ';' after expression.");
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
        self.advance();

        let prefix_rule: Option<ParseFn> = self.previous_rule().prefix_rule;
        let can_assign = precedence.as_u8() <= PrecAssignment.as_u8();

        match prefix_rule {
            Some(rule) => rule(self, can_assign),
            None => self.error("Expect expression."),
        }

        while precedence.as_u8() <= self.current_rule().precedence.as_u8() {
            self.advance();
            if let Some(infix_rule) = self.previous_rule().infix_rule {
                infix_rule(self, can_assign);
            }
        }

        if (can_assign) && self.match_(TokenEqual) {
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
            TokenAnd => todo!(),
            TokenAssert => todo!(),
            TokenBang => todo!(),
            TokenBangEqual => todo!(),
            TokenClass => todo!(),
            TokenComma => todo!(),
            TokenDot => todo!(),
            TokenElse => todo!(),
            TokenEof => todo!(),
            TokenEqual => todo!(),
            TokenEqualEqual => todo!(),
            TokenError => todo!(),
            TokenFalse => todo!(),
            TokenFor => todo!(),
            TokenFun => todo!(),
            TokenGreater => todo!(),
            TokenGreaterEqual => todo!(),
            TokenIdentifier => todo!(),
            TokenIf => todo!(),
            TokenLeftBrace => todo!(),
            TokenLeftParen => todo!(),
            TokenLess => todo!(),
            TokenLessEqual => todo!(),
            TokenMinus => todo!(),
            TokenNil => todo!(),
            TokenNumber => ParseRule {
                prefix_rule: Some(number),
                infix_rule: None,
                precedence: PrecAssignment,
            },
            TokenOr => todo!(),
            TokenPlus => todo!(),
            TokenPrint => todo!(),
            TokenReturn => todo!(),
            TokenRightBrace => todo!(),
            TokenRightParen => todo!(),
            TokenSemicolon => ParseRule {
                prefix_rule: None,
                infix_rule: None,
                precedence: PrecNone,
            },
            TokenSlash => todo!(),
            TokenStar => todo!(),
            TokenString => todo!(),
            TokenSuper => todo!(),
            TokenThis => todo!(),
            TokenTrue => todo!(),
            TokenVar => todo!(),
            TokenWhile => todo!(),
        }
    }

    fn copy_string(&mut self, start: usize, end: usize) -> usize {
        let string = ObjString(self.scanner.copy_segment(start, end));
        self.vm.memory.allocate(string)
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        if self.function.constants.len() >= 256 {
            todo!("error handling for over-full constant table")
        } else {
            self.function.constants.push(value);
            self.function.constants.len() as u8
        }
    }

    fn error_at_current(&mut self, message: &str) {
        todo!("emit a compiler error ({}) and continue", message)
    }

    fn error(&mut self, _message: &str) {
        todo!("emit a compiler error without locaiton")
    }

    fn synchronize(&mut self) {
        todo!("recover after an error")
    }

    fn end_compiler(mut self) -> Function {
        self.emit_return();

        if DEBUG_PRINT_CODE {
            self.function.disassemble_chunk();
        }

        self.function
    }
}

/// Section: parse functions
fn number(this: &mut Compiler, _can_assign: bool) {
    let start = this.parser.previous.start;
    let length = this.parser.previous.length;
    let value = convert(this.scanner.copy_segment(start, length));

    let index = this.make_constant(Value::Number(value));
    this.emit_bytes(OpConstant as u8, index);
}

fn convert(number: String) -> f64 {
    BigDecimal::from_str_radix(&number, 10)
        .unwrap()
        .to_f64()
        .unwrap()
}

impl Function {
    fn disassemble_chunk(&self) {
        todo!("debug print this function")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn compile_expression(expr: &str) -> (Function, VM) {
        let mut vm = VM::default();
        let compiler = Compiler::new(&mut vm);

        (compiler.compile(expr).unwrap(), vm)
    }

    #[test]
    fn test_compile_variable_declaration() {
        let (bytecode, mut vm) = compile_expression("var x;");

        assert_eq!(
            bytecode.chunk,
            vec![OpNil as u8, OpDefineGlobal as u8, 1, OpReturn as u8]
        );

        assert_eq!(1, bytecode.constants.len());

        if let Object(ptr) = bytecode.constants.get(0).unwrap() {
            if let ObjString(s) = vm.memory.retrieve(*ptr) {
                assert_eq!(s, "x");
            } else {
                panic!("expected memory to contian string variable name")
            }
        } else {
            panic!("expected the constant table to point to memory")
        }
    }

    #[test]
    fn test_compile_simple_integer_expression() {
        let (bytecode, _vm) = compile_expression("1;");

        assert_eq!(
            bytecode.chunk,
            vec![OpConstant as u8, 1, OpPop as u8, OpReturn as u8]
        );
    }
}
impl Precedence {
    fn as_u8(&self) -> u8 {
        match self {
            PrecNone => 0,
            PrecAssignment => 1,
            _PrecOr => 2,
            _PrecAnd => 3,
            _PrecEquality => 4,
            _PrecComparison => 5,
            _PrecTerm => 6,
            _PrecFactor => 7,
            _PrecUnary => 8,
            _PrecCall => 9,
            _PrecPrimary => 10,
        }
    }
}
