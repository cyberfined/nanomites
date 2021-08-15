#pragma once

#include <stddef.h>
#include "htable.h"
#include "file_map.h"

typedef struct patch_node {
    size_t  offset;
    int32_t nanocall_index;
    struct patch_node *next;
} patch_node;

patch_node* parse_elf(file_map *file, htable_t *offsets);
void patch_free(patch_node *patch_list);
