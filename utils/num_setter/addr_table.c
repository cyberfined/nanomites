#include "addr_table.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static uint32_t addr_node_hash(hnode_t *_node) {
    addr_node *node = (addr_node*)_node;
    return default_hash_func((uint8_t*)node->fun_name, node->fun_name_length);
}

static bool addr_node_keyeq(hnode_t *_node1, hnode_t *_node2) {
    addr_node *node1 = (addr_node*)_node1;
    addr_node *node2 = (addr_node*)_node2;
    return !strcmp(node1->fun_name, node2->fun_name);
}

static void addr_node_free(hnode_t *_node) {
    addr_node *node = (addr_node*)_node;
    free(node->fun_name);
    vector_free(node->addrs);
}

htable_t* addr_table_create(size_t max_nodes) {
    return htable_create(sizeof(addr_node), max_nodes, addr_node_hash, addr_node_keyeq, addr_node_free);
}

static inline bool addr_node_create(char *fun_name, addr_node *node) {
    size_t fun_name_length = strlen(fun_name);

    node->fun_name_length = fun_name_length;
    node->fun_name = strndup(fun_name, fun_name_length);
    node->addrs_offset = 0;
    if(!node->fun_name) {
        perror("addr_node_create");
        goto error;
    }

    node->addrs = vector_new(2, sizeof(long));
    if(!node->addrs)
        goto error;

    return true;
error:
    if(node->fun_name) free(node->fun_name);
    if(node->addrs) vector_free(node->addrs);
    return false;
}

bool addr_node_insert(htable_t *table, char *fun_name, long addr) {
    addr_node key = {
        .fun_name        = fun_name,
        .fun_name_length = strlen(fun_name),
    };
    bool is_new_node = false;

    addr_node *node = (addr_node*)htable_lookup(table, (hnode_t*)&key);
    addr_node new_node;
    if(!node) {
        is_new_node = true;
        if(!addr_node_create(fun_name, &new_node))
            return false;
        node = &new_node;
    }

    if(!vector_append(node->addrs, &addr)) {
        if(is_new_node)
            addr_node_free((hnode_t*)node);
        return false;
    }

    if(is_new_node && !htable_insert(table, (hnode_t*)node)) {
        addr_node_free((hnode_t*)node);
        return false;
    }

    return true;
}
