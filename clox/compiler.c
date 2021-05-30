
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,    // =
    PREC_OR,            // or
    PREC_AND,           // and
    PREC_EQUALITY,      // == !=
    PREC_COMPARISON,    // < > <= >=
    PREC_TERM,          // + -
    PREC_FACTOR,        // * /
    PREC_UNARY,         // ! -
    PREC_CALL,          // ()
    PREC_PRIMARY,
} Precedence;

typedef void (*ParseFn)(bool canAssign);

// # A single row in the parse table
//
// This struct stores the rules for each kind of token in
// the langauge. A key thing to note is that `prefix` stores
// the rules for literals like strings and numbers as well as
// prefix operators like negative. There are two columns, prefix
// and infix, for operators like `-` that can be both, and we
// decide which one to use based on whether we just started a
// new expression or we are in the middle of an expression.
typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

// # The struct that tracks local variables in scope
//
// The compiler struct tracks the local variables, the depth of the
// current scope, and the number of locals we currently have. This
// is opposed to tracking a linked list/stack of hash maps in jlox.
// (https://github.com/arlindohall/rlox/blob/main/src/interpreter.rs#L34) 
//
// Internal type of Local is a name and a resolution depth
typedef struct {
    Token name;
    int depth;
} Local;

typedef struct {
    Local locals[UINT8_COUNT];
    int localCount;
    int scopeDepth;
} Compiler;

// We could initialize these in the language runtime, and then pass each
// reference into the calling funciton. That would allow us to have
// multiple threads with separate memory. It would also require a
// refactor that I (and the book) don't want to do.
Parser parser;
Compiler* current = NULL;
Chunk* compilingChunk;

static void initCompiler(Compiler*);
static void advance();
static void expression();
static void statement();
static void declaration();
static bool match(TokenType type);
static void consume(TokenType, const char*);
static void endCompiler();

static void emitReturn();
static void emitByte(uint8_t);
static void emitBytes(uint8_t, uint8_t);

static uint8_t parseVariable(const char*);
static void defineVariable(uint8_t);

static uint8_t makeConstant(Value);
static uint8_t identifierConstant(Token*);

static void parsePrecedence(Precedence);
static ParseRule* getRule(TokenType);

static Chunk* currentChunk();

static void error(const char*);
static void errorAtCurrent(const char*);
static void errorAt(Token*, const char*);

// # Compile
//
// This is the entry point for the whole interpreter. Strings of Lox are taken
// in and converted to code chunks that the VM can operate on. In the process,
// we also parse constant values to push onto the constants stack.
bool compile(const char* source, Chunk* chunk) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.hadError = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    endCompiler();

    return !parser.hadError;
}

/// # Error handling
///
/// This section contains functions for dealing with error tokens that were found
/// during the scanning stage of the interpreter.
static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;
    parser.panicMode = true;

    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

/// # Bytecode emitter
///
/// This section contains functions for emtting bytes to the current chunk of
/// bytecode.
static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static Chunk* currentChunk() {
    return compilingChunk;
}

static void endCompiler() {
    emitReturn();
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

static void emitReturn() {
    emitByte(OP_RETURN);
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void initCompiler(Compiler* compiler) {
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    current = compiler;
}

static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t) constant;
}

/// # Compiler control functions
///
/// This section is made of functions that control the flow of the compiler/parser.
/// They either move the current token forward, check the surrounding tokens, or
/// dispatch to some sub-section of the parser to handle a kind of token.
static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}


// # Block statements
//
// Blocks just execute the contained statements in order. The
// interesting (while simple) bit is the scope functions below.
// In jlox, we popped or added scopes to a stack, but here we
// just track the scope level and have variables track their
// own depth. So creating or closing a scope just means updating
// the tracked scope depth, an integer.
static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    while (current->localCount > 0 &&
            current->locals[current->localCount - 1].depth > current->scopeDepth) {
        emitByte(OP_POP);
        current->localCount--;
    }
}

static void varDeclaration() {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    // Variables are defined by their index in the constant pool
    defineVariable(global);
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void assertStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after assertion.");
    emitByte(OP_ASSERT);
}

static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            case TOKEN_ASSERT:
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;
            default:
                ; // Do nothing.
        }

        advance();
    }
}

static void declaration() {
    if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

// Var declaration statements to come
static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_ASSERT)) {
        assertStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void string(bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
                                    parser.previous.length - 2)));
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t arg = identifierConstant(&name);

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_GLOBAL, arg);
    } else {
        emitBytes(OP_GET_GLOBAL, arg);
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // Compile the operand
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction
    switch (operatorType) {
        case TOKEN_BANG: emitByte(OP_NOT); break;
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        default:
            return;
    }
}

static void binary(bool canAssign) {
    // Remember the operator
    TokenType operatorType = parser.previous.type;

    // Compile the right operand
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    // Emit the operator instruction
    switch (operatorType) {
        case TOKEN_BANG_EQUAL:      emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL:     emitByte(OP_EQUAL); break;
        case TOKEN_GREATER:         emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL:   emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS:            emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:      emitBytes(OP_GREATER, OP_NOT); break;
        case TOKEN_PLUS:            emitByte(OP_ADD); break;
        case TOKEN_MINUS:           emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:            emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:           emitByte(OP_DIVIDE); break;
        default:
            return; // Unreachable
    }
}

// Note: this does not include number literals, which are handled by
// the special-purpose `number` function. This is dispatched to by the
// parsing table
static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE:   emitByte(OP_FALSE); break;
        case TOKEN_NIL:     emitByte(OP_NIL);   break;
        case TOKEN_TRUE:    emitByte(OP_TRUE);  break;
        default: return; // Unreachable
    }
}

/// # Parser rules
///
/// This is the part of the code that dispatches to get the
/// prefix and infix parsing functions and their precedence
/// based on the token.
ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
  [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ASSERT]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
    advance();

    // We call `getRule(...)->prefix` only when we first enter the
    // parsePrecedence function because it takes anything that is
    // the beginning of an expression, including literals.
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    // We search for `getRule(...)->infix` later because we have to
    // have already parsed a full expression to get to an infix
    // operator. If we don't find any, we just quit.
    //
    // We also rely on the precedence of the previous rule to
    // determine whether to fully parse this operator yet, or to
    // just keep searching.
    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

// # Create a global variable name constant
//
// Because global variables are looked up by name at runtime, we
// need to track variable names as constants. This could be an issue
// if we have a lot of global variables, we'll start ot eat into
// the constant storage. Maybe we'll revisit this?
static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    // Fine to use the name because it comes form the source chunk.
    // Once the compiling is done, we don't need the tokens anymore,
    // unless we build some kind of stack tracing in. Therefore it's
    // okay to drop them then and depend on them before we drop them
    // (them being the source names).
    local->name = name;
    local->depth = current->scopeDepth;
}

// # Check if two variables have the same name
//
// Don't just use the interned string compare because we haven't
// interned these strings
static bool identifierEquals(Token* a, Token* b) {
    if (a->length != b->length) return false; // Quick check and bail
    return memcmp(a->start, b->start, a->length) == 0;
}

static void declareVariable() {
    if (current->scopeDepth == 0) return; // Global variable

    Token* name = &parser.previous;
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != UNINITIALIZED_SENTINEL_DEPTH &&
                local->depth < current->scopeDepth) {
            // This is from the parent scope, so stop
            break;
        }

        if (identifierEquals(name, &local->name)) {
            error("Already variable with this name in this scope.");
        }
    }
    addLocal(*name);
}

static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0) {
        // It's a local variable, and we don't have to put the
        // value anywehre. This is pretty cool! We can just use
        // the value we pushed to the stack!
        return;
    }

    // Pushes the the value at the top of the stack to the
    // globals variable named by the constant pool location ref'ed
    // by `global`
    emitBytes(OP_DEFINE_GLOBAL, global);
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}
