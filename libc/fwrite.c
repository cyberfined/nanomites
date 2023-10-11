#include "lib.h"

__attribute__((always_inline))
static inline bool write_all(int fd, const char *buf, size_t size) {
    ssize_t wbytes = 0;
    do {
        wbytes = write(fd, buf, size);
        if(wbytes < 0)
            return false;
        size -= wbytes;
    } while(size != 0);
    return true;
}

size_t fwrite(const void* ptr, size_t size, size_t nmemb, FILE *f) {
    size *= nmemb;
    if(!write_all(f->fd, ptr, size))
        return 0;
    return size;
}
