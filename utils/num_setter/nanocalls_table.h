#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

typedef struct {
    long long addr;
    void      *nanocall;
} nanocall_node;

typedef struct {
    nanocall_node *nodes;
    size_t        num_nodes;
    size_t        max_nodes;
    int           size_ind;
} nanocalls_table;

nanocalls_table* nanocalls_create(size_t max_nodes);
bool nanocalls_insert(nanocalls_table *nanocalls, long long addr, void *nanocall);
void nanocalls_free(nanocalls_table *nanocalls);
