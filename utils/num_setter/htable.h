#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

typedef struct {
    uint32_t hash;
} hnode_t;

typedef uint32_t (*htable_hash_func)(hnode_t*);
typedef bool (*htable_keyeq_func)(hnode_t*, hnode_t*);
typedef void (*htable_free_func)(hnode_t*);
typedef struct {
    hnode_t **nodes;
    size_t  node_size;
    size_t  num_nodes;
    size_t  max_nodes;
    int     size_ind;

    htable_hash_func  hash_func;
    htable_keyeq_func keyeq_func;
    htable_free_func  free_func;
} htable_t;

uint32_t default_hash_func(const uint8_t *key, size_t length);

htable_t* htable_create(size_t node_size, size_t max_nodes, htable_hash_func hash_func, htable_keyeq_func keyeq_func, htable_free_func free_func);
bool htable_insert(htable_t *htable, hnode_t *node);
hnode_t* htable_lookup(htable_t *htable, hnode_t *node);
void htable_free(htable_t *htable);
