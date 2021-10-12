use TokenType::*;

use crate::vm::LoxError;
use crate::vm::LoxErrorChain;
use crate::vm::LoxErrorType::*;

/// The type of token that was scanned.
///
/// This is not a structure enum because every token has the same properties
/// so there's no need for separate structures.
#[derive(Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum TokenType {
    TokenAnd,
    TokenAssert,
    TokenBang,
    TokenBangEqual,
    TokenClass,
    TokenComma,
    TokenDot,
    TokenElse,
    TokenEof,
    TokenEqual,
    TokenEqualEqual,
    TokenError,
    TokenFalse,
    TokenFor,
    TokenFun,
    TokenGreater,
    TokenGreaterEqual,
    TokenIdentifier,
    TokenIf,
    TokenLeftBrace,
    TokenLeftParen,
    TokenLess,
    TokenLessEqual,
    TokenMinus,
    TokenNil,
    TokenNumber,
    TokenOr,
    TokenPlus,
    TokenPrint,
    TokenReturn,
    TokenRightBrace,
    TokenRightParen,
    TokenSemicolon,
    TokenSlash,
    TokenStar,
    TokenString,
    TokenSuper,
    TokenThis,
    TokenTrue,
    TokenVar,
    TokenWhile,
}

/// The actual token scanned by the scanner/parser.
///
/// This struct tracks the type of token, so we can match on the type,
/// the source of the token (used for error reporting and for named constants),
/// and the line number (used for error reporting).
#[derive(Clone, Debug)]
pub struct Token {
    pub type_: TokenType,
    pub start: usize,
    pub length: usize,
    pub line: usize,
}

/// The default token is used when initializing the parser.
impl Default for Token {
    fn default() -> Self {
        Token {
            type_: TokenNil,
            start: 0,
            length: 0,
            line: 0,
        }
    }
}

/// Scanner for turning a lox lang string into a list of tokens.
///
/// Produces a [Vec] of tokens that can be consumed by the
/// parser/compiler and turned into bytecode, that way we
/// don't have to produce bytecode directly from the text.
#[derive(Debug)]
pub struct Scanner {
    pub source: Vec<char>,

    start: usize,
    current: usize,

    line: usize,

    error_chain: LoxErrorChain,
}

impl Default for Scanner {
    fn default() -> Self {
        Scanner {
            source: Self::strip_str(""),
            start: 0,
            current: 0,
            line: 1,

            error_chain: LoxErrorChain::new(),
        }
    }
}

impl Scanner {
    pub fn take_str(&mut self, text: &str) {
        self.source = Self::strip_str(text);
    }

    fn strip_str(text: &str) -> Vec<char> {
        text.chars().collect()
    }

    pub fn copy_segment(&self, start: usize, end: usize) -> String {
        self.source[start..end].iter().collect()
    }

    /// Scan a single token from the source and return.
    ///
    /// This is where we ignore whitespace and scan identifiers,
    /// special characters, reserved words, and number/string literals.
    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();

        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenEof);
        }

        let c = self.advance();

        if Self::is_alpha(self.previous()) {
            self.identifier()
        } else if Self::is_digit(self.previous()) {
            self.number()
        } else {
            match c {
                '(' => self.make_token(TokenLeftParen),
                ')' => self.make_token(TokenRightParen),
                '{' => self.make_token(TokenLeftBrace),
                '}' => self.make_token(TokenRightBrace),
                ';' => self.make_token(TokenSemicolon),
                ',' => self.make_token(TokenComma),
                '.' => self.make_token(TokenDot),
                '-' => self.make_token(TokenMinus),
                '+' => self.make_token(TokenPlus),
                '/' => self.make_token(TokenSlash),
                '*' => self.make_token(TokenStar),
                '!' => {
                    if self.match_('=') {
                        self.make_token(TokenBangEqual)
                    } else {
                        self.make_token(TokenBang)
                    }
                }
                '=' => {
                    if self.match_('=') {
                        self.make_token(TokenEqualEqual)
                    } else {
                        self.make_token(TokenEqual)
                    }
                }
                '<' => {
                    if self.match_('=') {
                        self.make_token(TokenLessEqual)
                    } else {
                        self.make_token(TokenLess)
                    }
                }
                '>' => {
                    if self.match_('=') {
                        self.make_token(TokenGreaterEqual)
                    } else {
                        self.make_token(TokenGreater)
                    }
                }
                '"' => self.string(),
                '\0' => self.make_token(TokenEof),
                _ => panic!("Unrecognized character {}", self.previous()),
            }
        }
    }

    /// Helper to create a token from the last scanned item.
    fn make_token(&self, type_: TokenType) -> Token {
        Token {
            type_,
            start: self.start,
            length: self.current - self.start,
            line: self.line,
        }
    }

    /// Scan an identifier.
    ///
    /// Identifiers can be variable names, class names, or functions.
    /// Anything alphanumeric token that is not a reserved word. This
    /// helper also returns a special token when it crosses an reserved
    /// word of the type of that word.
    fn identifier(&mut self) -> Token {
        loop {
            if !(Self::is_alpha(self.peek()) || Self::is_digit(self.peek())) {
                break;
            } else {
                self.advance();
            }
        }

        self.make_token(self.identifier_type())
    }

    /// Either gives the type of identifier, or the type of the reserved word last scanned.
    fn identifier_type(&self) -> TokenType {
        match self.peek_nth(self.start) {
            'a' => {
                if self.current - self.start > 1 {
                    match self.peek_nth(self.start + 1) {
                        'n' => self.check_keyword(2, "d", TokenAnd),
                        's' => self.check_keyword(2, "sert", TokenAssert),
                        _ => TokenIdentifier,
                    }
                } else {
                    TokenIdentifier
                }
            }
            'c' => self.check_keyword(1, "lass", TokenClass),
            'e' => self.check_keyword(1, "lse", TokenElse),
            'f' => {
                if self.current - self.start > 1 {
                    match self.peek_nth(self.start + 1) {
                        'a' => self.check_keyword(2, "lse", TokenFalse),
                        'o' => self.check_keyword(2, "r", TokenFor),
                        'u' => self.check_keyword(2, "n", TokenFun),
                        _ => TokenIdentifier,
                    }
                } else {
                    TokenIdentifier
                }
            }
            'i' => self.check_keyword(1, "f", TokenIf),
            'n' => self.check_keyword(1, "il", TokenNil),
            'o' => self.check_keyword(1, "r", TokenOr),
            'p' => self.check_keyword(1, "rint", TokenPrint),
            'r' => self.check_keyword(1, "eturn", TokenReturn),
            's' => self.check_keyword(1, "uper", TokenSuper),
            't' => {
                if self.current - self.start > 1 {
                    match self.peek_nth(self.start + 1) {
                        'h' => self.check_keyword(2, "is", TokenThis),
                        'r' => self.check_keyword(2, "ue", TokenTrue),
                        _ => TokenIdentifier,
                    }
                } else {
                    TokenIdentifier
                }
            }
            'v' => self.check_keyword(1, "ar", TokenVar),
            'w' => self.check_keyword(1, "hile", TokenWhile),
            _ => TokenIdentifier,
        }
    }

    /// If the token pointed to the the parameter `start` matches the passed string, return that type.
    ///
    /// If the token isn't matched, then return `TokenIdentifier` because this is just
    /// a variable class or method name.
    fn check_keyword(&self, start: usize, matches: &str, type_: TokenType) -> TokenType {
        let mut idx = 0;
        for ch in matches.chars() {
            if ch != self.peek_nth(self.start + start + idx) {
                return TokenIdentifier;
            }
            idx += 1;
        }

        type_
    }

    fn number(&mut self) -> Token {
        while Self::is_digit(self.peek()) && !self.is_at_end() {
            self.advance();
        }

        if '.' == self.peek() && Self::is_digit(self.peek_next()) {
            self.advance();

            while Self::is_digit(self.peek()) {
                self.advance();
            }
        }

        self.make_token(TokenNumber)
    }

    fn string(&mut self) -> Token {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1
            }
            self.advance();
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }

        self.advance();
        self.make_token(TokenString)
    }

    fn match_(&mut self, c: char) -> bool {
        if self.is_at_end() {
            false
        } else if self.peek_next() != c {
            false
        } else {
            self.current += 1;
            true
        }
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

    fn is_alpha(c: char) -> bool {
        match c {
            'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n'
            | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | '_' => true,
            _ => false,
        }
    }

    fn is_digit(c: char) -> bool {
        match c {
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => true,
            _ => false,
        }
    }

    /// Skip ahead to the next character, and return the current one.
    ///
    /// The caller should want to take ownership of the current character
    /// and shouldn't try to use any of the other helpers to access it. Once
    /// this method is called, any call to `peek`, `previous`, or `peek_next`
    /// are done in the context of the character _after_ the one that's
    /// returned here.
    fn advance(&mut self) -> char {
        self.current += 1;
        self.previous()
    }

    /// The character most recently scanned (probably by a call to `advance`).
    fn previous(&self) -> char {
        self.peek_nth(self.current - 1)
    }

    /// The current character.
    ///
    /// If you call this after advance, it returns the character after the one returned by `advance`.
    ///
    /// If you call it somewhere in the middle of logic that iterates through some
    /// context in the program text, it's best to think of this as the next un-processed
    /// character in the text.
    fn peek(&self) -> char {
        self.peek_nth(self.current)
    }

    /// Look ahead (used especially in processing comments).
    fn peek_next(&self) -> char {
        self.peek_nth(self.current + 1)
    }

    /// Gives the character at index `i` from the _start_ of the program text.
    ///
    /// We rely on the Optional return of `&[T]::get` so we don't worry if we've actually
    /// requested a value beyond the source.
    fn peek_nth(&self, i: usize) -> char {
        *self.source.get(i).unwrap_or(&'\0')
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn error_token(&mut self, message: &'static str) -> Token {
        // todo: test that a scan error appears as one
        let message = message.to_string();
        self.error_chain.register(ScanError(LoxError {
            line: self.line,
            message,
        }));

        self.make_token(TokenError)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[cfg(test)]
    fn scanner_of(text: &str) -> Scanner {
        Scanner {
            source: Scanner::strip_str(text),
            start: 0,
            current: 0,
            line: 1,
            error_chain: LoxErrorChain::new(),
        }
    }

    #[test]
    fn test_scan_a_single_number_expression_statement() {
        let mut scanner = scanner_of("1;");

        assert_eq!(TokenNumber, scanner.scan_token().type_);
        assert_eq!(TokenSemicolon, scanner.scan_token().type_);
        assert_eq!(TokenEof, scanner.scan_token().type_);
    }

    #[test]
    fn test_scan_a_single_string_expression_statement() {
        let mut scanner = scanner_of("\"Hello, world!\";");

        assert_eq!(TokenString, scanner.scan_token().type_);
        assert_eq!(TokenSemicolon, scanner.scan_token().type_);
        assert_eq!(TokenEof, scanner.scan_token().type_);
    }

    #[test]
    fn test_scan_a_print_statement() {
        let mut scanner = scanner_of("print \"greetings\";");

        assert_eq!(TokenPrint, scanner.scan_token().type_);
        assert_eq!(TokenString, scanner.scan_token().type_);
        assert_eq!(TokenSemicolon, scanner.scan_token().type_);
        assert_eq!(TokenEof, scanner.scan_token().type_);
    }

    #[test]
    fn test_scan_variable_declaration() {
        let mut scanner = scanner_of("var x;");

        assert_eq!(TokenVar, scanner.scan_token().type_);
        assert_eq!(TokenIdentifier, scanner.scan_token().type_);
        assert_eq!(TokenSemicolon, scanner.scan_token().type_);
        assert_eq!(TokenEof, scanner.scan_token().type_);
    }

    #[test]
    fn test_scan_for_statement() {
        let mut scanner = scanner_of(
            "
        var x = 10;
        for (var y = 0; y < 10; y = y + 1) {
            print y;
        }
        ",
        );

        let token_types = vec![
            TokenVar,
            TokenIdentifier,
            TokenEqual,
            TokenNumber,
            TokenSemicolon,
            TokenFor,
            TokenLeftParen,
            TokenVar,
            TokenIdentifier,
            TokenEqual,
            TokenNumber,
            TokenSemicolon,
            TokenIdentifier,
            TokenLess,
            TokenNumber,
            TokenSemicolon,
            TokenIdentifier,
            TokenEqual,
            TokenIdentifier,
            TokenPlus,
            TokenNumber,
            TokenRightParen,
            TokenLeftBrace,
            TokenPrint,
            TokenIdentifier,
            TokenSemicolon,
            TokenRightBrace,
            TokenEof,
        ];

        for type_ in token_types {
            assert_eq!(type_, scanner.scan_token().type_);
        }
    }

    #[test]
    fn test_scan_function_declaration() {
        let text = "
        fun f(x) {
            return 10 + x;
        }
        ";
        let mut scanner = scanner_of(text);

        let token_types = vec![
            TokenFun,
            TokenIdentifier,
            TokenLeftParen,
            TokenIdentifier,
            TokenRightParen,
            TokenLeftBrace,
            TokenReturn,
            TokenNumber,
            TokenPlus,
            TokenIdentifier,
            TokenSemicolon,
            TokenRightBrace,
        ];

        for type_ in token_types {
            assert_eq!(type_, scanner.scan_token().type_);
        }
    }
}
