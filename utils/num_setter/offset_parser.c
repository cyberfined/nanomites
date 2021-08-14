#include "offset_parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "htable.h"

htable_t* parse_offsets(const char *filename) {
    FILE *inp = NULL;
    htable_t *table = NULL;
    char buf[256];
    int offset;
    bool error = true;

    inp = fopen(filename, "r");
    if(!inp) {
        perror("parse_offsets");
        goto cleanup;
    }

    table = offset_table_create(8);
    if(!table)
        goto cleanup;

    while(fgets(buf, sizeof(buf)-1, inp)) {
        if(strncmp(buf, "#define", 7) != 0)
            continue;

        char *token = strtok(buf, " ");
        char *beg = NULL, *end;
        while(token != NULL) {
            if(!beg && !strncmp("NANO_", token, 5)) {
                beg = token;
                end = strrchr(beg, '_');
                for(char *c = beg; c < end; c++)
                    *c = tolower(*c);
            } else if(beg) {
                *end = 0;
                offset = strtol(token, NULL, 10);
                if(!offset_node_insert(table, beg, offset))
                    goto cleanup;
            }

            token = strtok(NULL, " ");
        }
    }

    error = false;
cleanup:
    if(inp) fclose(inp);
    if(error && table) htable_free(table);
    return table;
}
