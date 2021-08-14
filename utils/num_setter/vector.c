#include "vector.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

vector_t* vector_new(size_t capacity, size_t elem_size) {
    vector_t *vec = malloc(sizeof(vector_t));
    if(!vec) {
        perror("vector_new");
        return NULL;
    }

    vec->length = 0;
    vec->capacity = capacity;
    vec->elem_size = elem_size;
    vec->data = calloc(capacity, elem_size);
    if(!vec->data) {
        perror("vector_new");
        free(vec);
        return NULL;
    }

    return vec;
}

bool vector_append(vector_t *vec, void *value) {
    if(vec->length >= vec->capacity) {
        vec->capacity <<= 1;
        size_t new_capacity = vec->capacity << 1;
        long *new_data = realloc(vec->data, new_capacity * vec->elem_size);
        if(!new_data) {
            perror("vector_append");
            return false;
        }
        vec->capacity = new_capacity;
        vec->data = new_data;
    }

    memcpy(vec->data + vec->length * vec->elem_size, value, vec->elem_size);
    vec->length++;
    return true;
}

void vector_free(vector_t *vec) {
    free(vec->data);
    free(vec);
}
