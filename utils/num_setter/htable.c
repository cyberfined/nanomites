#include "htable.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static size_t primes[] = {13, 31, 61, 103, 229, 523, 1093, 2239, 4519, 9043, 18121, 36343, 72673, 145513, 291043, 582139, 1164433};

uint32_t default_hash_func(const uint8_t *key, size_t length) {
    size_t i = 0;
    uint32_t hash = 0;
    while (i != length) {
        hash += key[i++];
        hash += hash << 10;
        hash ^= hash >> 6;
    }
    hash += hash << 3;
    hash ^= hash >> 11;
    hash += hash << 15;
    return hash;
}

htable_t* htable_create(size_t node_size, size_t max_nodes, htable_hash_func hash_func, htable_keyeq_func keyeq_func, htable_free_func free_func) {
    htable_t *htable = malloc(sizeof(htable_t));
    if(!htable) {
        perror("malloc");
        return NULL;
    }

    bool size_found = false;
    for(int i = 0; i < sizeof(primes)/sizeof(*primes); i++) {
        if(primes[i] >= max_nodes) {
            htable->max_nodes = primes[i];
            htable->size_ind = i;
            size_found = true;
            break;
        }
    }

    if(!size_found) {
        fprintf(stderr, "size %lu is too big\n", max_nodes);
        free(htable);
        return NULL;
    }

    htable->nodes = calloc(htable->max_nodes, sizeof(hnode_t*));
    if(!htable->nodes) {
        perror("calloc");
        free(htable);
        return NULL;
    }

    htable->node_size = node_size;
    htable->num_nodes = 0;
    htable->hash_func = hash_func;
    htable->keyeq_func = keyeq_func;
    htable->free_func = free_func;

    return htable;
}

bool htable_insert(htable_t *htable, hnode_t *node) {
    int cur, step, nstep;
    hnode_t *cur_node, *new_node;

    if(htable->num_nodes == htable->max_nodes) {
        int new_ind = htable->size_ind+1;
        size_t old_size, new_size;

        if(new_ind == sizeof(primes)/sizeof(*primes)) {
            fprintf(stderr, "Failed to insert: size %lu is maximum\n", htable->max_nodes);
            return false;
        }
        new_size = primes[new_ind];
        old_size = htable->max_nodes;
        hnode_t **new_nodes = calloc(new_size, sizeof(hnode_t*));
        if(!new_nodes) {
            perror("calloc");
            return false;
        }

        for(size_t i = 0; i < old_size; i++) {
            cur_node = htable->nodes[i];
            cur = cur_node->hash % new_size;
            step = cur_node->hash % (new_size-2) + 1;
            nstep = step;
            for(size_t j = 0; j < new_size; j++) {
                if(!new_nodes[cur]) {
                    new_nodes[cur] = cur_node;
                    break;
                }
                cur = (cur_node->hash + nstep) % new_size;
                nstep += step;
            }
        }
        free(htable->nodes);

        htable->size_ind = new_ind;
        htable->max_nodes = new_size;
        htable->nodes = new_nodes;
    }

    new_node = malloc(htable->node_size);
    if(!new_node) {
        perror("malloc");
        return false;
    }
    memcpy(new_node, node, htable->node_size);
    new_node->hash = htable->hash_func(node);

    bool is_inserted = false;

    cur = new_node->hash % htable->max_nodes;
    step = new_node->hash % (htable->max_nodes-2) + 1;
    nstep = step;
    for(size_t i = 0; i < htable->max_nodes; i++) {
        cur_node = htable->nodes[cur];
        if(!cur_node) {
            htable->nodes[cur] = new_node;
            htable->num_nodes++;
            is_inserted = true;
            break;
        }
        if(cur_node->hash == new_node->hash &&
           htable->keyeq_func(cur_node, new_node)) {
            if(htable->free_func)
                htable->free_func(cur_node);
            free(cur_node);
            htable->nodes[cur] = new_node;
            is_inserted = true;
            break;
        }

        cur = (new_node->hash + nstep) % htable->max_nodes;
        nstep += step;
    }

    if(!is_inserted) {
        fputs("Internal error: failed to insert node\n", stderr);
        return false;
    }

    return true;
}

hnode_t* htable_lookup(htable_t *htable, hnode_t *node) {
    uint32_t hash = htable->hash_func(node);
    int cur = hash % htable->max_nodes;
    int step = hash % (htable->max_nodes-2) + 1;
    int nstep = step;
    hnode_t *cur_node;

    for(size_t i = 0; i < htable->max_nodes; i++) {
        cur_node = htable->nodes[cur];
        if(!cur_node)
            break;
        if(cur_node->hash == hash &&
           htable->keyeq_func(cur_node, node))
            return cur_node;
        cur = (hash + nstep) % htable->max_nodes;
        nstep += step;
    }

    return NULL;
}

void htable_free(htable_t *htable) {
    for(size_t i = 0; i < htable->max_nodes; i++) {
        if(!htable->nodes[i])
            continue;
        if(htable->free_func)
            htable->free_func(htable->nodes[i]);
        free(htable->nodes[i]);
    }
    free(htable->nodes);
    free(htable);
}
