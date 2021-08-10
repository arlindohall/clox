use TokenType::*;

/// The type of token that was scanned.
///
/// This is not a structure enum because every token has the same properties
/// so there's no need for separate structures.
#[derive(Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum TokenType {
    TokenBang,
    TokenBangEqual,
    TokenClass,
    TokenComma,
    TokenDot,
    TokenEof,
    TokenEqual,
    TokenEqualEqual,
    TokenError,
    TokenFun,
    TokenGreater,
    TokenGreaterEqual,
    TokenLeftBrace,
    TokenLeftParen,
    TokenLess,
    TokenLessEqual,
    TokenMinus,
    TokenPlus,
    TokenRightBrace,
    TokenRightParen,
    TokenSemicolon,
    TokenSlash,
    TokenStar,
    TokenVar,
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
            line: 0,
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

        if self.is_alpha(c) {
            self.identifier()
        } else if self.is_digit(c) {
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
                _ => todo!("incomplete token match"),
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
        todo!("scan an identifier name")
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

    fn is_alpha(&self, _c: char) -> bool {
        todo!("check if this is an alpha character")
    }

    fn is_digit(&self, _c: char) -> bool {
        todo!("check if the current character is a digit")
    }

    fn advance(&mut self) -> char {
        todo!("increment parser one character")
    }

    fn peek(&self) -> char {
        self.source.chars().nth(self.current).unwrap()
    }

    fn peek_next(&self) -> char {
        todo!("get the next character")
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}
