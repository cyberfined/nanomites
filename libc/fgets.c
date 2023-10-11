#include "lib.h"

static FILE _stdin = {0, {0}, {0}, 0, 0};
FILE *stdin = &_stdin;

char* fgets(char *buf, size_t size, FILE *f) {
    size_t cpy_size = 0;

    for(;;) {
        if(f->r_size > 0) {
            char *newline = strchr(&f->rbuf[f->r_offset], '\n');
            if(newline) {
                cpy_size = newline - f->rbuf - f->r_offset + 1;
                if(cpy_size >= size)
                    cpy_size = size - 1;
                goto ret;
            } else if(f->r_offset + f->r_size >= sizeof(f->rbuf) - 1) {
                if(f->r_offset == 0) {
                    cpy_size = f->r_size;
                    if(cpy_size >= size)
                        cpy_size = size - 1;
                    goto ret;
                } else {
                    memmove(f->rbuf, &f->rbuf[f->r_offset], f->r_size);
                    f->r_offset = 0;
                }
            }
        }

        ssize_t rb = read(
            f->fd,
            &f->rbuf[f->r_offset + f->r_size],
            sizeof(f->rbuf) - f->r_offset - f->r_size
        );
        if(rb == 0) {
            return NULL;
        } else if(rb < 0) {
            // TODO: set error
            return NULL;
        }
        f->r_size += rb;

        continue;
ret:
        memcpy(buf, &f->rbuf[f->r_offset], cpy_size);
        buf[cpy_size] = 0;
        f->r_offset += cpy_size;
        f->r_size -= cpy_size;
        if(f->r_size == 0)
            f->r_offset = 0;
        break;
    }

    return buf;
}
