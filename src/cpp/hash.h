#pragma once

#include "gc.h"
#include "header.h"

/// This header:
/// struct Map;
/// Map*map_insert(Map*,SinObj*,SinObj*);
/// SinObj*map_keys(Map*);
/// SinObj*map_get(Map*,SinObj*);
extern "C" {
/// Map is defined as a linked list of keys and values.
/// Not hashed at all, thats too hard... Oh well.
typedef struct Map {
    Map* next;
    SinObj* key;
    SinObj* value;
} Map;


Map* insert_update(Map* m, SinObj* k, SinObj* v);
Map* insert_copy(Map* m);
Map* map_insert(Map* m, SinObj* k, SinObj* v);
SinObj* map_keys(Map* m);
SinObj* map_get(Map* m, SinObj* key);
bool map_has_key(Map* m, SinObj* key);
u64 map_count(Map* m);

// hash hash-keys hash-ref hash-set

Map* insert_update(Map* m, SinObj* k, SinObj* v) {
    if (m == NULL) {
        // made it all the way through the map and didnt find the val
        // So just add the entry to update at the end of the map.
        Map* final_val = reinterpret_cast<Map*>(GC_MALLOC(sizeof(Map)));
        final_val->next = NULL;
        final_val->key = k;
        final_val->value = v;
        return final_val;
    }
    Map* new_map = reinterpret_cast<Map*>(GC_MALLOC(sizeof(Map)));
    if (eq_helper(m->key, k) == 1) {
        new_map->key = k;
        new_map->value = v;
        // dont need to check for an update anymore
        new_map->next = insert_copy(m->next);
    } else {
        new_map->key = m->key;
        new_map->value = m->value;
        // need to keep looking for the value to update.
        new_map->next = insert_update(m->next, k, v);
    }
    return new_map;
}

// Shallowly copies each element into a new Map*.
Map* insert_copy(Map* m) {
    if (m == NULL) {
        return NULL;
    }
    Map* new_map = reinterpret_cast<Map*>(GC_MALLOC(sizeof(Map)));
    new_map->key = m->key;
    new_map->value = m->value;
    new_map->next = insert_copy(m->next);
    return new_map;
}

/// Inserts an Entry into the map m.
Map* map_insert(Map* m, SinObj* k, SinObj* v) {
    return insert_update(m, k, v);
}


/// Returns a Vector SinObj
/// Where the 0th spot is the size, and the others are the keys.
SinObj* map_keys(Map* m) {
    if (m == NULL) {
        return const_init_null();
    }

    SinObj* car = (m->key);
    SinObj* cdr = map_keys(m->next);
    SinObj* cons = prim_cons(car, cdr);
    return cons;
}

/// Returns a SinObj that is the value of the map.
/// If the key is not found, NULL is returned.
SinObj* map_get(Map* m, SinObj* key) {
    while (m != NULL) {
        if (eq_helper(key, m->key) == 1) {
            return m->value;
        }
        m = m->next;
    }
    return NULL;
}


bool map_has_key(Map* m, SinObj* key) {
    while (m != NULL) {
        if (eq_helper(m->key, key) == 1) {
            return true;
        }
        m = m->next;
    }
    return false;
}


u64 map_count(Map* m) {
    u64 count = 0;

    while (m != NULL) {
        count++;
        m = m->next;
    }

    return count;
}

} // end extern "C"
