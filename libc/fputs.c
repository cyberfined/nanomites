#include "lib.h"

int fputs(const char *s, FILE *f) {
    size_t len = strlen(s);
    return write(f->fd, s, len);
}

int puts(const char *s) {
    size_t len = strlen(s);
    if(len < sizeof(stdout->wbuf)) {
        memcpy(stdout->wbuf, s, len);
        stdout->wbuf[len] = '\n';
        return write(stdout->fd, stdout->wbuf, len+1);
    }

    int res = write(stdout->fd, s, len);
    if(res != len)
        return res;

    res = write(stdout->fd, "\n", 1);
    if(res != 1)
        return len;
    return len + 1;
}
