#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include "elf_parser.h"
#include "offset_parser.h"
#include "file_map.h"

static const uint8_t patch_pattern[] = {0xb8, 0xef, 0xbe, 0xad, 0xde};

static inline bool patch_nanocalls(file_map *elf, patch_node *patch_list) {
    uint32_t pattern = *(uint32_t*)patch_pattern;
    uint8_t *data = elf->data;

    for(patch_node *patch = patch_list; patch != NULL; patch = patch->next) {
        bool is_found = false;
        size_t offset;
        for(offset = patch->offset; offset >= sizeof(patch_pattern); offset--) {
            if(*(uint32_t*)&data[offset] == pattern) {
                is_found = true;
                break;
            }
        }

        if(!is_found) {
            fprintf(stderr, "Failed to patch nanocall at offset %p\n", (void*)patch->offset);
            return false;
        }

        *(int32_t*)&data[offset+1] = patch->nanocall_index;
    }
    return true;
}

int main(int argc, char **argv) {
    if(argc != 4) {
        fprintf(stderr,
                "Usage: %s <executable> <table.h> <out>\n"
                "Util for set magical numbers for nanosyscalls\n",
                argv[0]);
        return 1;
    }

    const char *executable = argv[1];
    const char *offsets_file = argv[2];
    const char *output = argv[3];

    patch_node *patch_list = NULL;
    htable_t *offsets = NULL;
    file_map *elf = NULL;
    int res = 1;

    elf = file_map_open(executable);
    if(!elf)
        goto cleanup;

    offsets = parse_offsets(offsets_file);
    if(!offsets)
        goto cleanup;

    patch_list = parse_elf(elf, offsets);
    if(!patch_list)
        goto cleanup;

    if(!patch_nanocalls(elf, patch_list))
        goto cleanup;

    if(!file_map_write(elf, output))
        goto cleanup;

    res = 0;
cleanup:
    if(elf) file_map_destroy(elf);
    if(patch_list) patch_free(patch_list);
    if(offsets) htable_free(offsets);
    return res;
}
