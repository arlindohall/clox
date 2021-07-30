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
    parser: Parser,
}

/// Scanner for turning a lox lang string into a list of tokens.
///
/// Produces a [Vec] of tokens that can be consumed by the
/// parser/compiler and turned into bytecode, that way we
/// don't have to produce bytecode directly from the text.
#[derive(Debug)]
struct Scanner<'a> {
    source: &'a str,
}

/// The parser that does all the work creating the bytecode.
#[derive(Debug)]
struct Parser {
    had_error: bool
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

use Token::*;
enum Token {
    TokenEof
}

impl <'a> Compiler<'a> {
    /// Create a new compiler with empty source and no errors.
    ///
    /// This method also initializes the scanner and parser, and
    /// is fine to use any time we need a new [Compiler]
    pub fn new(vm: &'a VM) -> Compiler<'a> {
        Compiler {
            vm,
            scanner: Scanner {
                source: ""
            },
            parser: Parser {
                had_error: false
            }
        }
    }

    /// Compile the statement given and report errors back to the VM.
    ///
    /// The statement passed in can be a group of statements separated
    /// by a ';' character, as specified in Lox grammar.
    pub fn compile(&mut self, statement: &'a str) -> Result<Function, Box<dyn Error>> {
        self.scanner = Scanner {
            source: statement,
        };
        self.parser = Parser {
            had_error: false,
        };

        self.advance();

        while !self.match_(TokenEof) {
            self.declaration();
        }


        Ok(self.end_compiler())
    }

    fn advance(&mut self) {}
    fn match_(&self, _token: Token) -> bool { true }
    fn declaration(&mut self) {}
    fn end_compiler(&mut self) -> Function { Function {} }
}
