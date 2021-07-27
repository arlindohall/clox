
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

/// # Data structures

/// ## Parser
///
/// Main data structure of the compiler (which the book stresses
/// is really just a parser that outputs bytecode instead of
/// syntax trees).
///
/// Contains a pointer to the current and last tokens in the
/// scanned source text for convenience. Also tracks whether
/// there has ever been a compiler error or we are currently
/// panicing and trying to recover.
typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

/// Precedences are used to determine whether to keep
/// parsing the current expression or break and start
/// a sub-expression, to determine the grouping of otherwise
/// ambiguous expressions.
///
/// For example `1 == a and 2 == b` is the same as
/// `(1 == a) and (2 == b) because `and` has a lower
/// precedence than `==`.
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

/// Functios used in the parse table
typedef void (*ParseFn)(bool canAssign);

/// # A single row in the parse table
///
/// This struct stores the rules for each kind of token in
/// the langauge. A key thing to note is that `prefix` stores
/// the rules for literals like strings and numbers as well as
/// prefix operators like negative. There are two columns, prefix
/// and infix, for operators like `-` that can be both, and we
/// decide which one to use based on whether we just started a
/// new expression or we are in the middle of an expression.
typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

/// # The struct that tracks local variables in scope
///
/// The compiler struct tracks the local variables, the depth of the
/// current scope, and the number of locals we currently have. This
/// is opposed to tracking a linked list/stack of hash maps in jlox.
/// (https://github.com/arlindohall/rlox/blob/main/src/interpreter.rs#L34) 
///
/// Internal type of Local is a name and a resolution depth
typedef struct {
    Token name;
    int depth;
    bool isCaptured;
} Local;

typedef struct {
    uint8_t index;
    bool isLocal;
} Upvalue;

typedef enum {
    TYPE_INITIALIZER,
    TYPE_FUNCTION,
    TYPE_METHOD,
    TYPE_SCRIPT
} FunctionType;

/// ## Compiler struct
///
/// This is the primary data structure for the compiler,
/// and one key thing to note is that it is a linked list
/// of compilers, terminated by `NULL`. Each compiler points
/// to its lexical parent. So a new compiler for a function
/// points back to the script context
typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFunction* function;
    FunctionType type;

    Local locals[UINT8_COUNT];
    int localCount;
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    bool hasSuperclass;
} ClassCompiler;

/// We could initialize these in the language runtime, and then pass each
/// reference into the calling funciton. That would allow us to have
/// multiple threads with separate memory. It would also require a
/// refactor that I (and the book) don't want to do.
Parser parser;
Compiler* current = NULL;
ClassCompiler* currentClass = NULL;

/// # Prototypes

/// Pulled up because it depends on the table, which in turn
/// depends on the rest of the functions being defined
static ParseRule* getRule(TokenType);

/// Pulled up because its definition references a few kinds
/// of statements, but each of those can contain other statements,
/// so we want to be able to access this in the specific functions.
/// For example, `whileStatement`, `ifStatement`.
static void statement();

/// Pulled up so we can refernce declarations inside blocks,
/// while we also reference blocks inside functions, and
/// functions inside declarations.
static void declaration();

/// Pulled up because I made a mistake in the method ordering
/// compared to the book, and I don't want to go back and
/// figure out what order to put things in yet.
static void namedVariable(Token, bool);

/// # Error handling
///
/// This section contains functions for dealing with error tokens that were found
/// during the scanning stage of the interpreter.
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

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static Chunk* currentChunk() {
    return &current->function->chunk;
}

/// # Bytecode emitter
///
/// This section contains functions for emtting bytes to the current chunk of
/// bytecode.

/// All the functions that emit bytes rely on the helpers that
/// do the actual emitting.
static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    // Emit two dummy bytes that we will have to patch later,
    // and make a note of where the two bytes start (the location
    // of the `instruction` above), so we can come back and put
    // the final jump address there when we've compiled the
    // `then` block
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

/// ## Patch jump
///
/// Assume that we've already emitted a jump and the following
/// few bytes of bytecode that we want to (maybe) jump over.
/// Then we'll use this helper to peek backwards by `offset`,
/// modify the location pointed to by that jump instruction
/// to right here (the current token), and ensure that the jump
/// is aligned correctly over those two bytes.
static void patchJump(int offset) {
    // -2 to adjust for the bytecode for the jump itself
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

static void emitReturn() {
    if (current->type == TYPE_INITIALIZER) {
        // The first local variable is "this", the object that's
        // the the referent of the call
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);
    }

    emitByte(OP_RETURN);
}

/// # Compiler control functions
///
/// This section is made of functions that control the flow of the compiler/parser.
/// They either move the current token forward, check the surrounding tokens, or
/// dispatch to some sub-section of the parser to handle a kind of token.

static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;

    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(
            parser.previous.start,
            parser.previous.length
        );
    }

    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(
            currentChunk(),
            function->name != NULL ? function->name->chars : "<script>"
        );
    }
#endif

    // We assume here (obviously) that the `current`
    // has been initialized.
    current = current->enclosing;
    return function;
}

static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t) constant;
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

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

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    while (
        current->localCount > 0 &&
        current->locals[current->localCount - 1].depth > current->scopeDepth
    ) {
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }
        current->localCount--;
    }
}

static void markInitialized() {
    if (current->scopeDepth == 0) return; // Was called in global scope, should be a function
    current->locals[current->localCount - 1].depth =
            current->scopeDepth;
}

static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0) {
        // It's a local variable, and we don't have to put the
        // value anywehre. This is pretty cool! We can just use
        // the value we pushed to the stack!
        markInitialized();
        return;
    }

    // Pushes the the value at the top of the stack to the
    // globals variable named by the constant pool location ref'ed
    // by `global`
    emitBytes(OP_DEFINE_GLOBAL, global);
}

/// # Check if two variables have the same name
///
/// Don't just use the interned string compare because we haven't
/// interned these strings
static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false; // Quick check and bail
    return memcmp(a->start, b->start, a->length) == 0;
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
    local->depth = UNINITIALIZED_SENTINEL_DEPTH;
    local->isCaptured = false;
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

        if (identifiersEqual(name, &local->name)) {
            error("Already variable with this name in this scope.");
        }
    }
    addLocal(*name);
}

/// # Create a global variable name constant
///
/// Because global variables are looked up by name at runtime, we
/// need to track variable names as constants. This could be an issue
/// if we have a lot of global variables, we'll start ot eat into
/// the constant storage. Maybe we'll revisit this?
static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
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

static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();

    int elseJump = emitJump(OP_JUMP);

    patchJump(thenJump);
    emitByte(OP_POP);

    if (match(TOKEN_ELSE)) statement();
    patchJump(elseJump);
}

static void whileStatement() {
    int loopStart = currentChunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    // Pop the condition beginning of inside loop
    emitByte(OP_POP);
    statement();
    emitLoop(loopStart);

    patchJump(exitJump);
    // Pop the condition just after exiting the loop
    emitByte(OP_POP);
}

static void forStatement() {
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(TOKEN_SEMICOLON)) {
        // No initializer, continue
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;
    int exitJump = -1;
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition");

        // Jump out of the loop if the condition is false
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP); // Drop the condition if it was true
    }

    if (!match(TOKEN_RIGHT_PAREN)) {
        // Skip the increment code, go directly to the body
        // but keep track of the increment
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        // Perform the increment (we jump back here later)
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        // Increment is the last thing before jumping back to
        // the start of the loop.
        emitLoop(loopStart);
        loopStart = incrementStart;
        // Patch here because the next thing is the loop body
        patchJump(bodyJump);
    }

    statement();
    emitLoop(loopStart);

    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP); // Drop the condition if it was false
    }

    endScope();
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code");
    }

    if (match(TOKEN_SEMICOLON)) {
        emitReturn();
    } else {
        if (current->type == TYPE_INITIALIZER) {
            error("Can't return a value from an initializer.");
        }

        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
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

/// ## Block statements
///
/// Blocks just execute the contained statements in order. The
/// interesting (while simple) bit is the scope functions below.
/// In jlox, we popped or added scopes to a stack, but here we
/// just track the scope level and have variables track their
/// own depth. So creating or closing a scope just means updating
/// the tracked scope depth, an integer.
static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect '(' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

static void method() {
    consume(TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t constant = identifierConstant(&parser.previous);

    FunctionType type = TYPE_METHOD;
    if (
        parser.previous.length == 4
        && memcmp(parser.previous.start, "init", 4) == 0
    ) {
        type = TYPE_INITIALIZER;
    }

    function(type);
    emitBytes(OP_METHOD, constant);
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    Token className = parser.previous;
    uint8_t nameConstant = identifierConstant(&parser.previous);
    declareVariable();

    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant);

    ClassCompiler classCompiler;
    classCompiler.hasSuperclass = false;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    if (match(TOKEN_LESS)) {
        consume(TOKEN_IDENTIFIER, "Expect superclass name.");
        variable(false);

        if (identifiersEqual(&className, &parser.previous)) {
            error("A class can't inherit from itself.");
        }

        beginScope();
        addLocal(syntheticToken("super"));
        defineVariable(0);

        namedVariable(className, false);
        emitByte(OP_INHERIT);
        classCompiler.hasSuperclass = true;
    }

    namedVariable(className, false);
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(OP_POP);

    if (classCompiler.hasSuperclass) {
        endScope();
    }

    currentClass = currentClass->enclosing;
}

static void funDeclaration() {
    uint8_t global = parseVariable("Expect function name.");
    markInitialized();
    function(TYPE_FUNCTION);
    defineVariable(global);
}

static void declaration() {
    if (match(TOKEN_CLASS)) {
        classDeclaration();
    } else if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

/// # Compile
///
/// This is the entry point for the whole interpreter. Strings of Lox are taken
/// in and converted to code chunks that the VM can operate on. In the process,
/// we also parse constant values to push onto the constants stack.
ObjFunction* compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.hadError = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();

    return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
    Compiler* compiler = current;
    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}

static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_ASSERT)) {
        assertStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
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

/// Helper only used by namedVariable
static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        // Check each local variable (item on stack)
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == UNINITIALIZED_SENTINEL_DEPTH) {
                error("Can't read local variable in its own initializer");
            }
            return i;
        }
    }

    // Return the same value as if the local was uninitialized.
    // Note that this means not initializing a local variable
    // but then referencing it by name is only distinguished from
    // a reference to a global variable by whether an error was
    // reported above during resolution.
    return UNINITIALIZED_SENTINEL_DEPTH;
}

static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    // This was an absolute ghoul of a bug. I left off the `++`, which
    // meant the compiler wasn't ever adding any upvalues. It showed up
    // as a segfault whenever I tried to resolve the "0th" upvalue
    // because I had emitted an instruction to look there, but
    return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler* compiler, Token* name) {
    // Treat upvalue at top level as uninitialized value,
    // that is to say continue searching as a global variable
    // in the calling function. It could be missing at runtime,
    // but that's the VM's problem
    if (compiler->enclosing == NULL) return UNINITIALIZED_SENTINEL_DEPTH;

    int local = resolveLocal(compiler->enclosing, name);
    if (local != UNINITIALIZED_SENTINEL_DEPTH) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != UNINITIALIZED_SENTINEL_DEPTH) {
        // Super cool what we do here. The trick is to point
        // to the enclosing function's upvalue if it exists, which
        // means we can get a chain of upvalues for multi-layer
        // closures, and we don't have to worry about them being
        // lost because they won't be garbage collected!
        //
        // I don't know what happens when the value at the top level gets
        // kicked off the stack, though.
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return UNINITIALIZED_SENTINEL_DEPTH;
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != UNINITIALIZED_SENTINEL_DEPTH) {
        // Local variable exists and is initialized
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if (
        (arg = resolveUpvalue(current, &name))
        !=
        UNINITIALIZED_SENTINEL_DEPTH
    ) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, arg);
    } else {
        emitBytes(getOp, arg);
    }
}

static void this_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'this' outside of a class.");
        return;
    }

    variable(false);
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

static uint8_t argumentList() {
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if (argCount == 255) {
                error("Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

static void call(bool canAssign) {
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint8_t name = identifierConstant(&parser.previous);

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_PROPERTY, name);
    } else if (match(TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList();
        emitBytes(OP_INVOKE, name);
        emitByte(argCount);
    } else {
        emitBytes(OP_GET_PROPERTY, name);
    }
}

/// Note: this does not include number literals, which are handled by
/// the special-purpose `number` function. This is dispatched to by the
/// parsing table
static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE:   emitByte(OP_FALSE); break;
        case TOKEN_NIL:     emitByte(OP_NIL);   break;
        case TOKEN_TRUE:    emitByte(OP_TRUE);  break;
        default: return; // Unreachable
    }
}

static void and_(bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    // Pop the previous expression if it was false and
    // keep evaluating the rest of the expressions.
    emitByte(OP_POP);
    // Now evaluate the rest of the expression, possible
    // including more and expressions, before we finally
    // mark the point we're jumping to.
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

static void or_(bool canAssign) {
    // 1. We emit two jumps. The first is only triggered
    // if the prev value was false, and jumps to (2). The second
    // is triggered if the prev value was true, and jumps
    // to (3)
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    // 2. The first jump goes here, if the value was false,
    // we pop it and keep looking for false values with the
    // parse precedence below
    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    // 3. We jump past the parse precedence so we don't
    // evaluate more expressions if the value was true.
    // Here, we're not going to pop because either a function
    // call or print statement will use the value, or an
    // expression statement will drop it.
    patchJump(endJump);
}

/// # Parser rules
///
/// This is the part of the code that dispatches to get the
/// prefix and infix parsing functions and their precedence
/// based on the token.
ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
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
  [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
  [TOKEN_ASSERT]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {this_,    NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}
