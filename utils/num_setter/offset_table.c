#include "offset_table.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "htable.h"

static uint32_t offset_node_hash(hnode_t *_node) {
    offset_node *node = (offset_node*)_node;
    return default_hash_func((uint8_t*)node->call_name, node->call_name_length);
}

static bool offset_node_keyeq(hnode_t *_node1, hnode_t *_node2) {
    offset_node *node1 = (offset_node*)_node1;
    offset_node *node2 = (offset_node*)_node2;
    return !strcmp(node1->call_name, node2->call_name);
}

static void offset_node_free(hnode_t *_node) {
    offset_node *node = (offset_node*)_node;
    free(node->call_name);
}

htable_t* offset_table_create(size_t max_nodes) {
    htable_t *table = htable_create(sizeof(offset_node), max_nodes, offset_node_hash,
                                    offset_node_keyeq, offset_node_free);
    return table;
}

bool offset_node_insert(htable_t *table, char *call_name, int offset) {
    offset_node node = {
        .call_name        = strdup(call_name),
        .call_name_length = strlen(call_name),
        .offset           = offset,
    };

    if(!node.call_name) {
        perror("offset_node_insert");
        return false;
    }

    if(!htable_insert(table, (hnode_t*)&node)) {
        free(node.call_name);
        return false;
    }

    return true;
}
