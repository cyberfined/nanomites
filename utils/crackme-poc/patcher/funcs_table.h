#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

typedef struct {
    long long addr;
    uint32_t  size;
    uint8_t   data[16];
} func_node;

typedef struct {
    func_node *nodes;
    size_t    num_nodes;
    size_t    max_nodes;
    int       size_ind;
} funcs_table;

funcs_table* funcs_create(size_t max_nodes);
size_t get_func_table_size(size_t max_nodes);
bool funcs_insert(funcs_table *funcs, long long addr, uint32_t size, void *data);
void funcs_free(funcs_table *funcs);
