#pragma once

#include <stddef.h>
#include <stdbool.h>

typedef struct {
    size_t size;
    void   *data;
} file_map;

file_map* file_map_open(const char *filename);
bool file_map_write(file_map *m, const char *filename);
void file_map_destroy(file_map *m);
