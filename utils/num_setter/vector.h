#pragma once

#include <stddef.h>
#include <stdbool.h>

typedef struct {
    void   *data;
    size_t length;
    size_t capacity;
    size_t elem_size;
} vector_t;

vector_t* vector_new(size_t capacity, size_t elem_size);
bool vector_append(vector_t *vec, void *value);
void vector_free(vector_t *vec);
