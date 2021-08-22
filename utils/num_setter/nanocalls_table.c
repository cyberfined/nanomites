#include "nanocalls_table.h"

#include <stdio.h>
#include <stdlib.h>

static size_t primes[] = {13, 31, 61, 103, 229, 523, 1093, 2239, 4519, 9043, 18121, 36343, 72673, 145513, 291043, 582139, 1164433};

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

nanocalls_table* nanocalls_create(size_t max_nodes) {
    nanocalls_table *nanocalls = malloc(sizeof(nanocalls_table));
    if(!nanocalls) {
        perror("malloc");
        return NULL;
    }

    bool size_found = false;
    for(int i = 0; i < sizeof(primes)/sizeof(*primes); i++) {
        if(primes[i] >= max_nodes) {
            nanocalls->max_nodes = primes[i];
            nanocalls->size_ind = i;
            size_found = true;
            break;
        }
    }

    if(!size_found) {
        fprintf(stderr, "size %lu is too big\n", max_nodes);
        free(nanocalls);
        return NULL;
    }

    nanocalls->nodes = calloc(nanocalls->max_nodes, sizeof(nanocall_node));
    if(!nanocalls->nodes) {
        perror("calloc");
        free(nanocalls);
        return NULL;
    }

    nanocalls->num_nodes = 0;
    return nanocalls;
}

bool nanocalls_insert(nanocalls_table *nanocalls, long long addr, void *nanocall) {
    int cur, step, nstep;
    if(nanocalls->num_nodes == nanocalls->max_nodes) {
        fprintf(stderr, "Max nodes number is %lu\n", nanocalls->max_nodes);
        return false;
    }

    bool is_inserted = false;
    uint32_t hash = jenkins_hash_func(addr);

    cur = hash % nanocalls->max_nodes;
    step = hash % (nanocalls->max_nodes-2) + 1;
    nstep = step;
    for(size_t i = 0; i < nanocalls->max_nodes; i++) {
        nanocall_node *cur_node = &nanocalls->nodes[cur];
        if(!cur_node->addr) {
            nanocalls->nodes[cur] = (nanocall_node) {
                .addr     = addr,
                .nanocall = nanocall
            };
            nanocalls->num_nodes++;
            is_inserted = true;
            break;
        }
        cur = (hash + nstep) % nanocalls->max_nodes;
        nstep += step;
    }

    if(!is_inserted) {
        fputs("Internal error: failed to insert node\n", stderr);
        return false;
    }

    return true;
}

void nanocalls_free(nanocalls_table *nanocalls) {
    free(nanocalls->nodes);
    free(nanocalls);
}
