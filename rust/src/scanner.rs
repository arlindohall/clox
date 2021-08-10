use TokenType::*;

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
    TokenOr,
    TokenPlus,
    TokenPrint,
    TokenReturn,
    TokenRightBrace,
    TokenRightParen,
    TokenSemicolon,
    TokenSlash,
    TokenStar,
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
    start: usize,
    length: usize,
    line: usize,
}

/// The default token is used when initializing the parser.
impl Default for Token {
    fn default() -> Self {
        Token {
            type_: TokenEof,
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
pub struct Scanner<'a> {
    pub source: &'a str,

    start: usize,
    current: usize,

    line: usize,
}

impl<'a> Default for Scanner<'a> {
    fn default() -> Self {
        Scanner {
            source: "",
            start: 0,
            current: 0,
            line: 1,
        }
    }
}

impl<'a, 'b> Scanner<'a> {
    /// Scan a single token from the source into the scanner.
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

    fn make_token(&self, type_: TokenType) -> Token {
        Token {
            type_,
            start: self.start,
            length: self.current - self.start,
            line: self.line,
        }
    }

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

    fn check_keyword(&self, start: usize, matches: &str, type_: TokenType) -> TokenType {
        let mut idx = 0;
        for ch in matches.chars() {
            if ch != self.peek_nth(start + idx) {
                return TokenIdentifier;
            }
            idx += 1;
        }

        type_
    }

    fn number(&mut self) -> Token {
        todo!("scan a number constant")
    }

    fn string(&mut self) -> Token {
        todo!("scan a string literal")
    }

    fn match_(&mut self, _c: char) -> bool {
        todo!("check if the next char is c")
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

    fn advance(&mut self) -> char {
        self.current += 1;
        self.previous()
    }

    fn previous(&self) -> char {
        self.peek_nth(self.current - 1)
    }

    fn peek(&self) -> char {
        self.source.chars().nth(self.current).unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        self.source.chars().nth(self.current + 1).unwrap_or('\0')
    }

    fn peek_nth(&self, i: usize) -> char {
        self.source.chars().nth(i).unwrap_or('\0')
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_scan_variable_declaration() {
        let mut scanner = Scanner {
            source: "var x;",
            start: 0,
            current: 0,
            line: 1,
        };

        assert_eq!(TokenVar, scanner.scan_token().type_);
        assert_eq!(TokenIdentifier, scanner.scan_token().type_);
        assert_eq!(TokenSemicolon, scanner.scan_token().type_);
        assert_eq!(TokenEof, scanner.scan_token().type_);
    }
}
