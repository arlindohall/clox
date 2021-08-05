use std::error::Error;

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
    TokenEof,
    TokenError,
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
            },
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

    fn match_(&self, _type_: TokenType) -> bool {
        true
    }
    fn declaration(&mut self) {}
    fn end_compiler(&mut self) -> Function {
        Function {}
    }
    fn error_at_current(&mut self) {}
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
        loop {
            let c = self.peek();

            match c {
                ' ' | '\t' | '\r' => self.advance(),
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == '/' {
                        loop {
                            if self.peek() == '\n' || self.is_at_end() {
                                break;
                            } else {
                                self.advance();
                            }
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn advance(&mut self) {}
    fn peek(&self) -> char {
        ' '
    }
    fn peek_next(&self) -> char {
        ' '
    }
    fn is_at_end(&self) -> bool {
        true
    }
}
