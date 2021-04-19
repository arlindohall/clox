
#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

Parser parser;

Chunk* compilingChunk;

static void advance();
static void expression();
static void consume(TokenType, const char*);
static void endCompiler();

static void emitReturn();
static void emitByte(uint8_t);
static void emitBytes(uint8_t, uint8_t);

static uint8_t makeConstant(Value value);

static Chunk* currentChunk();

static void error(const char*);
static void errorAtCurrent(const char*);
static void errorAt(Token*, const char*);

bool compile(const char* source, Chunk* chunk) {
    initScanner(source);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.hadError = false;

    advance();
    expression();
    consume(TOKEN_EOF, "Expect end of expression.");
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
}

static void emitReturn() {
    emitByte(OP_RETURN);
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
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

static void expression() {
    // This is where the guts of the compiler go, but until it is implemented,
    // I think we're correctly going to get an error from running against any
    // expression/input.
}

static void number() {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(value);
}

static void grouping() {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}