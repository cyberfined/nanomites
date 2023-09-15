#include "funcs_table.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static size_t primes[] = {
    13, 31, 61, 103, 229, 523, 1093, 2239, 4519, 9043, 18121,
    36343, 72673, 145513, 291043, 582139, 1164433
};

static inline uint32_t jenkins_hash_func(long long addr) {
    size_t i = 0;
    uint32_t hash = 0;
    uint8_t *key = (uint8_t*)&addr;
    while (i != sizeof(addr)) {
        hash += key[i++];
        hash += hash << 10;
        hash ^= hash >> 6;
    }
    hash += hash << 3;
    hash ^= hash >> 11;
    hash += hash << 15;
    return hash;
}

size_t get_func_table_size(size_t max_nodes) {
    for(int i = 0; i < sizeof(primes)/sizeof(*primes); i++) {
        if(primes[i] >= max_nodes)
            return primes[i];
    }
    return 0;
}

funcs_table* funcs_create(size_t max_nodes) {
    funcs_table *funcs = malloc(sizeof(funcs_table));
    if(!funcs) {
        perror("malloc");
        return NULL;
    }

    bool size_found = false;
    for(int i = 0; i < sizeof(primes)/sizeof(*primes); i++) {
        if(primes[i] >= max_nodes) {
            funcs->max_nodes = primes[i];
            funcs->size_ind = i;
            size_found = true;
            break;
        }
    }

    if(!size_found) {
        fprintf(stderr, "size %lu is too big\n", max_nodes);
        free(funcs);
        return NULL;
    }

    funcs->nodes = calloc(funcs->max_nodes, sizeof(func_node));
    if(!funcs->nodes) {
        perror("calloc");
        free(funcs);
        return NULL;
    }

    funcs->num_nodes = 0;
    return funcs;
}

bool funcs_insert(funcs_table *funcs, long long addr, uint32_t size, void *data) {
    int cur, step, nstep;
    if(funcs->num_nodes == funcs->max_nodes) {
        fprintf(stderr, "Max nodes number is %lu\n", funcs->max_nodes);
        return false;
    }

    bool is_inserted = false;
    uint32_t hash = jenkins_hash_func(addr);

    cur = hash % funcs->max_nodes;
    step = hash % (funcs->max_nodes-2) + 1;
    nstep = step;
    for(size_t i = 0; i < funcs->max_nodes; i++) {
        func_node *cur_node = &funcs->nodes[cur];
        if(!cur_node->addr) {
            cur_node->addr = addr;
            cur_node->size = size;
            memcpy(&cur_node->data, data, sizeof(cur_node->data));
            funcs->num_nodes++;
            is_inserted = true;
            break;
        }
        cur = (hash + nstep) % funcs->max_nodes;
        nstep += step;
    }

    if(!is_inserted) {
        fputs("Internal error: failed to insert node\n", stderr);
        return false;
    }

    return true;
}

void funcs_free(funcs_table *funcs) {
    free(funcs->nodes);
    free(funcs);
}
