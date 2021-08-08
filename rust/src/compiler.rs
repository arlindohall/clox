use std::error::Error;

use crate::vm::Op::*;
use crate::vm::VM;

/// Compiler used for a single function or script.
///
/// The compiler needs a lifetime because it stores a reference to
/// the source string in `scanner.source`. The source string, though
/// isn't just the full program text passed in from the script interpreter,
/// it could also be the most recent line from the repl. In the latter
/// case, the source will be dropped on the next repl loop iteration.
#[derive(Debug)]
pub struct Compiler<'a> {
    vm: &'a VM<'a>,

    scanner: Scanner<'a>,
    parser: Parser<'a>,

    scope_depth: usize,
}

/// Scanner for turning a lox lang string into a list of tokens.
///
/// Produces a [Vec] of tokens that can be consumed by the
/// parser/compiler and turned into bytecode, that way we
/// don't have to produce bytecode directly from the text.
#[derive(Debug)]
struct Scanner<'a> {
    source: &'a str,

    start: usize,
    current: usize,

    line: usize,
}

/// The parser that does all the work creating the bytecode.
#[derive(Debug)]
struct Parser<'a> {
    had_error: bool,

    current: Token<'a>,
    previous: Token<'a>,

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
pub struct Function {}

use TokenType::*;

/// The type of token that was scanned.
///
/// This is not a structure enum because every token has the same properties
/// so there's no need for separate structures.
#[derive(Clone, Debug, PartialEq)]
#[repr(u8)]
enum TokenType {
    TokenClass,
    TokenEof,
    TokenError,
    TokenEqual,
    TokenFun,
    TokenSemicolon,
    TokenVar,
}

/// The actual token scanned by the scanner/parser.
///
/// This struct tracks the type of token, so we can match on the type,
/// the source of the token (used for error reporting and for named constants),
/// and the line number (used for error reporting).
#[derive(Clone, Debug)]
struct Token<'a> {
    type_: TokenType,
    source: &'a str,
    line: usize,
}

/// The default token is used when initializing the parser.
impl<'a> Default for Token<'a> {
    fn default() -> Self {
        Token {
            type_: TokenEof,
            source: "",
            line: 0,
        }
    }
}

impl<'a> Compiler<'a> {
    /// Create a new compiler with empty source and no errors.
    ///
    /// This method also initializes the scanner and parser, and
    /// is fine to use any time we need a new [Compiler]
    pub fn new(vm: &'a VM) -> Compiler<'a> {
        Compiler {
            vm,
            scanner: Scanner {
                source: "",
                start: 0,
                current: 0,
                line: 0,
            },
            parser: Parser {
                had_error: false,
                current: Default::default(),
                previous: Default::default(),
                panic_mode: false,
            },
            scope_depth: 0,
        }
    }

    /// Compile the statement given and report errors back to the VM.
    ///
    /// The statement passed in can be a group of statements separated
    /// by a ';' character, as specified in Lox grammar.
    pub fn compile(&mut self, statement: &'a str) -> Result<Function, Box<dyn Error>> {
        self.scanner.source = statement;
        self.parser.had_error = false;

        self.advance();

        while !self.match_(TokenEof) {
            self.declaration();
        }

        Ok(self.end_compiler())
    }

    /// Move the parser forward by one token.
    ///
    /// This calls out to the scanner (which the parser owns) to
    /// scan through the source, passing whitespace, until it has scanned
    /// a single token.
    fn advance(&mut self) {
        std::mem::swap(&mut self.parser.previous, &mut self.parser.current);
        self.parser.current = self.scanner.scan_token();

        loop {
            self.parser.current = self.scanner.scan_token();

            if self.parser.current.type_ != TokenError {
                break;
            }

            self.error_at_current();
        }
    }

    /// If the current (next to be compiled) token matches, then advance
    ///
    /// Return whether the token was matched, and the skip-ahead
    /// behavior is sort of hidden from the caller. But that lets the
    /// caller do cool things like the following:
    ///
    /// ```ignore
    /// let compiler = Compiler::new();
    /// if compiler.match_(TokenEof) {
    ///     println!("End of file");
    /// }
    /// ```
    fn match_(&mut self, type_: TokenType) -> bool {
        self.check(type_) || {
            self.advance();
            false
        }
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

    fn consume(&self, _token_type: TokenType, _message: &str) {
        todo!("advance one token and discard")
    }

    fn expression(&self) {
        todo!("compile one expression to bytecode (putting on stack)")
    }

    fn emit_bytes(&self, _op1: u8, _op2: u8) {
        todo!("emit two bytes of bytecode")
    }

    fn emit_byte(&self, _op: u8) {
        todo!("emit one byte")
    }

    fn parse_variable(&mut self, _message: &str) -> u8 {
        todo!("parse one variable name and return the constant table address")
    }

    fn class_declaration(&self) {
        todo!("compile a class definition along with methods")
    }

    fn fun_declaration(&self) {
        todo!("compile a single function definition")
    }

    fn statement(&self) {
        todo!("compile a single non-definition statement")
    }

    fn end_compiler(&mut self) -> Function {
        todo!("return this compiler's bytecode as a function object")
    }

    fn error_at_current(&mut self) {
        todo!("emit a compiler error and continue")
    }

    fn synchronize(&mut self) {
        todo!("recover after an error")
    }
}

impl<'a, 'b> Scanner<'a> {
    /// Scan a single token from the source into the scanner.
    fn scan_token(&mut self) -> Token<'b> {
        self.skip_whitespace();

        self.start = self.current;

        if self.is_at_end() {
            return Token {
                type_: TokenEof,
                source: "",
                line: self.line,
            };
        }

        Default::default()
    }

    /// Move the pointer in the source string forward past whitespace.
    fn skip_whitespace(&mut self) {
        'whitespace: loop {
            let c = self.peek();

            match c {
                ' ' | '\t' | '\r' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == '/' {
                        'comment: loop {
                            if self.peek() == '\n' || self.is_at_end() {
                                break 'comment;
                            } else {
                                self.advance();
                            }
                        }
                    } else {
                        continue 'whitespace;
                    }
                }
                // The only place where we break is when something
                // is not whitespace
                _ => break 'whitespace,
            }
        }
    }

    fn advance(&mut self) -> char {
        todo!("increment parser one character")
    }

    fn peek(&self) -> char {
        todo!("get the current character")
    }

    fn peek_next(&self) -> char {
        todo!("get the next character")
    }

    fn is_at_end(&self) -> bool {
        todo!("check if the pointer is equal to the source length")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_match_end_of_file() {
        let mut vm = VM::new();
        vm.interpret("print \"Hello world\";");
    }
}
