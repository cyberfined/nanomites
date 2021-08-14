#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include "vector.h"
#include "htable.h"

typedef struct {
    uint32_t hash;
    char     *fun_name;
    size_t   fun_name_length;
    size_t   addrs_offset;
    vector_t *addrs;
} addr_node;

htable_t* addr_table_create(size_t max_nodes);
bool addr_node_insert(htable_t *table, char *fun_name, long addr);
