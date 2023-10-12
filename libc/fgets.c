#include "lib.h"

static FILE _stdin = {0, {0}, 0, 0};
FILE *stdin = &_stdin;

char* fgets(char *buf, size_t size, FILE *f) {
    size_t cpy_size = 0;

    for(;;) {
        if(f->size > 0) {
            char *newline = strchr(&f->buf[f->offset], '\n');
            if(newline) {
                cpy_size = newline - f->buf - f->offset + 1;
                if(cpy_size >= size)
                    cpy_size = size - 1;
                goto ret;
            } else if(f->offset + f->size >= sizeof(f->buf) - 1) {
                if(f->offset == 0) {
                    cpy_size = f->size;
                    if(cpy_size >= size)
                        cpy_size = size - 1;
                    goto ret;
                } else {
                    memmove(f->buf, &f->buf[f->offset], f->size);
                    f->offset = 0;
                }
            }
        }

        ssize_t rb = read(
            f->fd,
            &f->buf[f->offset + f->size],
            sizeof(f->buf) - f->offset - f->size
        );
        if(rb == 0) {
            return NULL;
        } else if(rb < 0) {
            // TODO: set error
            return NULL;
        }
        f->size += rb;

        continue;
ret:
        memcpy(buf, &f->buf[f->offset], cpy_size);
        buf[cpy_size] = 0;
        f->offset += cpy_size;
        f->size -= cpy_size;
        if(f->size == 0)
            f->offset = 0;
        break;
    }

    return buf;
}
