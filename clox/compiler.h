
#ifndef clox_compiler_h
#define clox_compiler_h

#include "object.h"
#include "vm.h"

ObjFunction* compile(const char* source);

// Used to track uninitialized variables with the depth field of a `Local`
#define UNINITIALIZED_SENTINEL_DEPTH -1

#endif