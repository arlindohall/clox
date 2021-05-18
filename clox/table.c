
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
    uint32_t index = key->hash % capacity;
    Entry* tombstone = NULL;

    for (;;) {
        Entry* entry = &entries[index];
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) {
                // Empty entry--if we found a tombstone previously then
                // we can fill that value in instead of this one for
                // writing or I guess for reading, is that a bug?
                return tombstone != NULL ? tombstone : entry;
            } else {
                // We found a tombstone, so set it and keep looking
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (entry->key == key) {
            // This comparison is reliable because of string interning
            // see `Table strings` in [vm.h]
            return entry;
        }

        index = (index + 1) % capacity;
    }
}

// # Increase the size of a hash map
//
// Whenever we reach the load factor, call this to allocate new space
// and then copy each entry over to the new array.
static void adjustCapacity(Table* table, int capacity) {
    Entry* entries = ALLOCATE(Entry, capacity);

    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    // Reset the size so we don't copy tombstones
    table->count = 0;
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key == NULL) continue;

        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

// # Insert an item into the table, pass-by-value
//
// The set function starts by checking if the table is big enough
// to fit one more item without being more than 75% full (less then
// completely full because we'll start to see too many collisions
// even before it is full). If it's too full, re-size right away
// and copy every element over. Just like with the growable arrays
// we rely on the growing factor to smooth out pauses for growing
// so the average insert time is as if the array was always big enough.
//
// Then, we just look through the array at the hash, scanning until
// we find where the key (string) matches or there is nothing. If
// there's nothing there yet, we just increment the table size.
// In both cases, we just overwrite the key and value.
bool tableSet(Table* table, ObjString* key, Value value) {
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }
    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = entry->key == NULL;
    // Only increment count if we're not writing to a tombstone
    if (isNewKey && IS_NIL(entry->value)) table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

// # Get item from table
//
// Note that the return value reflects whether the item exists, not
// the item value itself.
bool tableGet(Table* table, ObjString* key, Value* value) {
    // In case the table is null, also optimizes for empty allocated table
    if (table->count == 0) return false;

    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    // Pass-by-reference by modifying pointer. Unsafe but workable
    *value = entry->value;
    return false;
}

// # Delete item from table
//
// This leaves a tombstone in place in the table to avoid the
// problem of dropped adjacent items. Like getting, we skip the
// case where the count is zero.
bool tableDelete(Table* table, ObjString* key) {
    if (table->count == 0) return false;

    // Find the entry to be deleted
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    // Tombstone is just a null key but with a value
    entry->key = NULL;
    entry->value = BOOL_VAL(true);

    return true;
}

void tableAddAll(Table* from, Table* to) {
    for (int i = 0; i < from->capacity; i++) {
        Entry* entry = &from->entries[i];
        if (entry->key != NULL) {
            tableSet(to, entry->key, entry->value);
        }
    }
}

// # Find a string from a table, without comparing pointers
//
// If we did a pointer compare here like in `tableGet`, we'd have
// a regression because we use this method when working with interning
// strings. So we need to actually copmare the whole string contents
// so that we can be sure the strings are unique.
ObjString* tableFindString(Table* table, const char* chars, int length,
                           uint32_t hash) {
    if (table->count == 0) return NULL;

    uint32_t index = hash % table->capacity;
    for (;;) {
        Entry* entry = &table->entries[index];
        if (entry->key == NULL) {
            // Stop if we find an empty non-tombstone
            if (IS_NIL(entry->value)) return NULL;
        } else if (entry->key->length == length &&
                entry->key->hash == hash &&
                memcmp(entry->key->chars, chars, length) == 0) {
            // We found it.
            return entry->key;
        }

        // I made a silly mistake and left this off at first---
        // don't do that, it will cause infinite loops on every
        // collision
        index = (index + 1) % table->capacity;
    }
}