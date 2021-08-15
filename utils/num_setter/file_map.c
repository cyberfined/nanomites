#include "file_map.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>

file_map* file_map_open(const char *filename) {
    file_map *m = NULL;
    int fd = -1;

    m = malloc(sizeof(file_map));
    if(!m)
        goto error;

    fd = open(filename, O_RDONLY);
    if(fd < 0)
        goto error;

    struct stat info;
    if(fstat(fd, &info) < 0)
        goto error;
    m->size = info.st_size;

    m->data = mmap(NULL, info.st_size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
    if(m->data == MAP_FAILED)
        goto error;
    close(fd);
    
    return m;
error:
    perror("file_map_open");
    if(m) free(m);
    if(fd >= 0) close(fd);
    return NULL;
}

bool file_map_write(file_map *m, const char *filename) {
    int fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0744);
    if(fd < 0 || write(fd, m->data, m->size) != m->size) {
        perror("file_map_write");
        return false;
    }
    close(fd);
    return true;
}

void file_map_destroy(file_map *m) {
    munmap(m->data, m->size);
    free(m);
}
