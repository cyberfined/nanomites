#pragma once

#include <stdint.h>
#include <stdbool.h>
#include "htable.h"

typedef struct {
    uint32_t hash;
    char     *call_name;
    size_t    call_name_length;
    int      offset;
} offset_node;

htable_t* offset_table_create(size_t max_nodes);
bool offset_node_insert(htable_t *table, char *call_name, int offset);
