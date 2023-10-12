#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

typedef struct {
    uint64_t addr;
    uint8_t  data[8];
} func_node;

typedef struct {
    func_node *nodes;
    size_t    num_nodes;
    size_t    max_nodes;
    int       size_ind;
} funcs_table;

funcs_table* funcs_create(size_t max_nodes);
size_t get_func_table_size(size_t max_nodes);
bool funcs_insert(funcs_table *funcs, uint64_t addr, void *data);
void funcs_free(funcs_table *funcs);
