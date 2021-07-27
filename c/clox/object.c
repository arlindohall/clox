
#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;

    object->next = vm.objects;
    vm.objects = object;

#ifdef DEBUG_LOG_GC
    printf("Allocate(ptr=%p, size=%zu, type=%d)\n", (void*)object, size, type);
#endif

    return object;
}

ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method) {
    ObjBoundMethod* bound = ALLOCATE_OBJ(
        ObjBoundMethod, OBJ_BOUND_METHOD
    );

    bound->receiver = receiver;
    bound->method = method;
    return bound;
}

ObjClass* newClass(ObjString* name) {
    ObjClass* clss = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
    clss->name = name;
    initTable(&clss->methods);
    return clss;
}

ObjClosure* newClosure(ObjFunction* function) {
    ObjUpvalue** upvalues = ALLOCATE(
        ObjUpvalue*,
        function->upvalueCount
    );

    for (int i = 0; i < function->upvalueCount; i++) {
        upvalues[i] = NULL;
    }

    ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvalueCount = function->upvalueCount;
    return closure;
}

ObjFunction* newFunction() {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->upvalueCount = 0;
    function->name = NULL;
    initChunk(&function->chunk);
    return function;
}

ObjInstance* newInstance(ObjClass* clss) {
    ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
    instance->clss = clss;
    initTable(&instance->fields);
    return instance;
}

ObjNative* newNative(NativeFn function) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;
    return native;
}

/// # Allocate a new string object
///
/// We expect the caller to check if the string is interned before calling.
/// That's why we're able to call tableSet without checking if it's there
/// first, and allocate a string every time we call.
static ObjString* allocateString(char* chars, int length, uint32_t hash) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    push(OBJ_VAL(string));
    tableSet(&vm.strings, string, NIL_VAL);
    pop();
    return string;
}

/// Yay FNV-1a :)
///
/// How does it work, or why, you ask? I have no idea, the book told me to.
static uint32_t hashString(const char* key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

/// # Use a reference to a string as the base for this
///
/// This is useful when creating a new string out of an existing interned
/// string because we added two strings together.
ObjString* takeString(char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);

    // If the string is interned, we don't need the passed in C string,
    // so we can just get rid of it and pass the interned pointer.
    if (interned != NULL) {
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }
    return allocateString(chars, length, hash);
}

ObjString* copyString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) return interned;

    char* heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    return allocateString(heapChars, length, hash);
}

ObjUpvalue* newUpvalue(Value* slot) {
    ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->closed = NIL_VAL;
    upvalue->location = slot;
    upvalue->next = NULL;
    return upvalue;
}

static void printFunction(ObjFunction* function) {
    if (function->name == NULL) {
        printf("Script");
    } else {
        printf("Function(%s)", function->name->chars);
    }
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_BOUND_METHOD: {
            ObjBoundMethod* bound = AS_BOUND_METHOD(value);
            printf("BoundMethod(callee=");
            printValue(bound->receiver);
            printf(", method=");
            printFunction(bound->method->function);
            printf(")");
            break;
        }
        case OBJ_CLASS:
            printf("%s", AS_CLASS(value)->name->chars);
            break;
        case OBJ_CLOSURE: {
            printf("Closure(function=");
            printFunction(AS_CLOSURE(value)->function);
            printf(")");
            break;
        }
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
        case OBJ_INSTANCE: {
            ObjInstance* instance = AS_INSTANCE(value);
            printf("%s(", instance->clss->name->chars);

            bool firstValuePrint = true;
            for (int i = 0; i < instance->fields.capacity; i++) {
                Entry entry = instance->fields.entries[i];
                if (entry.key == NULL) {
                    // Empty entry or tombstone
                } else {
                    if (firstValuePrint) {
                        // Haven't printed a value yet
                        firstValuePrint = false;
                    } else {
                        // In-between two values. Put a comma
                        printf(", ");
                    }
                    printf("%s=", entry.key->chars);
                    printValue(entry.value);
                }
            }

            printf(")");
        }
            break;
        case OBJ_NATIVE:
            printf("NativeFunction()");
            break;
        case OBJ_FUNCTION:
            printFunction(AS_FUNCTION(value));
            break;
        case OBJ_UPVALUE:
            printf("Upvalue");
            break;
    }
}
