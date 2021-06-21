
#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

#define OBJ_TYPE(value)         (AS_OBJ(value)->type)

#define IS_FUNCTION(value)      isObjType((value), OBJ_FUNCTION)
#define IS_STRING(value)        isObjType((value), OBJ_STRING)

#define AS_FUNCTION(value)      ((ObjFunction*)AS_OBJ(value))
#define AS_STRING(value)        ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)       (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_FUNCTION,
    OBJ_STRING,
} ObjType;

/// # Obj structure, contains Lox object
///
/// This structure serves two purposes. First, it is the header of
/// a string or other dynamically typed object, so that we can dereference
/// to the correct type and get the object value. Second, it is a node
/// in a linked list of references to every allocated value in the
/// program, which we can follow in order to perform garbage collection.
///
/// The linked list is held in the VM, updated in object.c whenever
/// a new object is allocated. Notice it is a singly-linked list and
/// so we use it like a stack.
struct Obj {
    ObjType type;
    struct Obj* next;
};

/// # Function
///
/// A function contains its own chunk of bytecode rather than
/// building into the parent chunk. This works because we can
/// keep all of the code separate and inspect functions in
/// a more modular way.
typedef struct {
    // Functions are first class so they have to start with Obj
    Obj obj;
    int arity;
    Chunk chunk;
    // Just for error reporting
    ObjString* name;
} ObjFunction;

struct ObjString {
    Obj obj;
    int length;
    char* chars;
    uint32_t hash; // Strings keep track of their hash values from creation
};

ObjFunction* newFunction();
ObjString* takeString(char*, int);
ObjString* copyString(const char* chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
