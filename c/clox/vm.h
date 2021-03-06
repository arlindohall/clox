
#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
    ObjClosure* closure;
    uint8_t* ip;
    Value* slots;
} CallFrame;

typedef struct {
    // The VM doesn't point directly to the top level chunk, but
    // instead points to all the call frames, which each point
    // to their own chunk (function, including top-level "script"
    // type functions).
    //
    // We also keep track of how many frames we've used so we can
    // error on overflow.
    CallFrame frames[FRAMES_MAX];
    int frameCount;

    Value stack[STACK_MAX];
    Value* stackTop;
    Table globals;
    Table strings;
    ObjString* initString;

    ObjUpvalue* openUpvalues;

    size_t bytesAllocated;
    size_t nextGC;
    // TODO: Replace ll object tracker with dynamic array
    Obj* objects; // Head of the linked list of objects for GC
    int grayCount;
    int grayCapacity;
    Obj** grayStack;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

/// We expose the VM to the outside world so that we can actually
/// view the object list elsewhere in the interpreter. Messy, but
/// effective.
extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif