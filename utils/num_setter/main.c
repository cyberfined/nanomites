#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <libgen.h>
#include "addr_table.h"
#include "elf_parser.h"
#include "offset_parser.h"

static inline bool func_name(char *dest, char *buf) {
    int len = strlen(buf);
    int i, j;

    if(buf[len-1] == '{' && buf[len-3] == ')') {
        for(i = len - 3; i >= 0 && buf[i] != '('; i--);
        for(j = i; j >= 0 && buf[j] != ' '; j--);
        j++;
        for(; j < i; j++) *dest++ = buf[j];
        *dest = 0;
        return true;
    }
    return false;
}

static inline char* strip(char *buf, int *zero_idx, char *c) {
    char *end = buf + strlen(buf) - 1;
    while(buf < end && isspace(*buf)) buf++;
    while(end > buf && isspace(*end)) end--;
    *c = end[1];
    *zero_idx = end - buf + 1;
    end[1] = 0;
    return buf;
}

static inline bool process_file(htable_t *nanocalls, htable_t *offsets, FILE *inp, FILE *out) {
    char buf[256];
    bool in_func = false;
    char func[64];
    addr_node addr_key = {.fun_name = func}, *func_node;
    offset_node offset_key;

    while(fgets(buf, sizeof(buf)-1, inp)) {
        int zero_idx = -1;
        char space;
        char *stripped_buf = strip(buf, &zero_idx, &space);
        if(!strncmp(stripped_buf, "nano_", 5)) {
            if(!in_func) {
                fprintf(stderr, "Failed to identify function for %s\n", stripped_buf);
                return false;
            }

            char *brace = strchr(buf, '(');
            if(!brace) {
                fprintf(stderr, "Something went wrong for %s\n", stripped_buf);
                return false;
            }

            *brace = 0;
            offset_key.call_name = stripped_buf;
            offset_key.call_name_length = strlen(stripped_buf);
            offset_node *off_node = (offset_node*)htable_lookup(offsets, (hnode_t*)&offset_key);
            if(!off_node) {
                fprintf(stderr, "Failed to get offset for %s function\n", stripped_buf);
                return false;
            }
            *brace = '(';

            if(zero_idx >= 0)
                stripped_buf[zero_idx] = space;

            fwrite(buf, 1, brace-buf+1, out);

            if(!func_node) {
                addr_key.fun_name_length = strlen(func);
                func_node = (addr_node*)htable_lookup(nanocalls, (hnode_t*)&addr_key);
                if(!func_node) {
                    fprintf(stderr, "Failed to get info for %s function", func);
                    return false;
                }
            }

            if(func_node->addrs->length <= func_node->addrs_offset) {
                fprintf(stderr, "Failed to get info for %s function", func);
                return false;
            }

            long *addrs = func_node->addrs->data;
            long addr = addrs[func_node->addrs_offset++];
            uint32_t hash = default_hash_func((uint8_t*)&addr, sizeof(addr));
            int magic_number = off_node->offset - hash % offsets->num_nodes;

            fprintf(out, "%d, ", magic_number);
            fputs(brace+1, out);

            continue;
        } else if(!in_func) {
            in_func = func_name(func, stripped_buf);
        }

        if(zero_idx >= 0)
            stripped_buf[zero_idx] = space;

        if(buf[0] == '}') {
            in_func = false;
            func_node = NULL;
        }

        fputs(buf, out);
    }

    return true;
}

int main(int argc, char **argv) {
    if(argc < 5) {
        fprintf(stderr,
                "Usage: %s <executable> <table.h> <outdir> <src1.c> [src2.c...]\n"
                "Util for set magical numbers for nanosyscalls\n",
                argv[0]);
        return 1;
    }

    char *executable = argv[1];
    char *offsets_file = argv[2];
    char *outdir = argv[3];

    htable_t *nanocalls = NULL, *offsets = NULL;
    FILE *inp = NULL, *out = NULL;
    int res = 1;

    nanocalls = parse_elf(executable);
    if(!nanocalls)
        goto cleanup;

    offsets = parse_offsets(offsets_file);
    if(!offsets)
        goto cleanup;

    char outfile[256];
    outdir = strcpy(outfile, outdir);
    outdir += strlen(outfile);
    *outdir++ = '/';
    *outdir = 0;

    for(int i = 4; i < argc; i++) {
        char *source = argv[i];
        char *bs = basename(source);
        strcat(outdir, bs);

        inp = fopen(source, "r");
        if(!inp) {
            perror("fopen");
            goto cleanup;
        }

        out = fopen(outfile, "w");
        if(!out) {
            perror("fopen");
            goto cleanup;
        }

        if(!process_file(nanocalls, offsets, inp, out))
            goto cleanup;

        fclose(inp);
        fclose(out);
        inp = out = NULL;
        *outdir = 0;
    }

    res = 0;
cleanup:
    if(nanocalls) htable_free(nanocalls);
    if(offsets) htable_free(offsets);
    if(inp) fclose(inp);
    if(out) fclose(out);
    return res;
}
