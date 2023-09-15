#include "lib.h"

static ssize_t write_all(int fd, char *buf, size_t size) {
    while(size != 0) {
        ssize_t wb = write(fd, buf, size);
        if(wb < 0)
            return wb;
        size -= wb;
    }
    return size;
}

static void print_usage(char *prog_name, char *buf) {
    size_t prog_name_len = strlen(prog_name);
    char *cur_char = buf;
    memcpy(cur_char, "Usage: ", 7);
    cur_char += 7;
    memcpy(cur_char, prog_name, prog_name_len);
    cur_char += prog_name_len;
    memcpy(cur_char, " -[uc]\n", 7);
    cur_char += 7;
    write_all(STDERR, buf, cur_char - buf);
}

typedef struct {
    int    fd;
    char   buf[4096];
    size_t offset;
    size_t size;
} FILE;

static FILE stdin = {0, {0}, 0, 0};

static char* strchr(char *s, char c) {
    while(*s) {
        if(*s == c)
            return s;
        s++;
    }
    return NULL;
}

static void memmove(void *_dst, void *_src, size_t size) {
    for(char *dst = _dst, *src = _src; size != 0; dst++, src++, size--) {
        *dst = *src;
    }
}

static char* fgets(char *buf, size_t size, FILE *f) {
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
            write_all(STDERR, "fgets: I/O error\n", 17);
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

static void reverse(char *buf, size_t size) {
    for(size_t i = 0, j = size - 1; i < j; i++, j--) {
        char c = buf[i];
        buf[i] = buf[j];
        buf[j] = c;
    }
}

static size_t itoa(size_t num, char *buf) {
    size_t i = 0;
    do {
        buf[i++] = num % 10 + '0';
        num /= 10;
    } while(num != 0);
    reverse(buf, i);
    return i;
}

int main(int argc, char **argv) {
    bool only_uniq = false, with_count = false;
    char num_buf[16], buf1[4096], buf2[4096] = {0};
    char *cur_line = buf1, *prev_line = buf2;

    if(argc > 3) {
        print_usage(argv[0], buf1);
        return 1;
    } 

    for(int i = 1; i < argc; i++) {
        if(!strcmp(argv[i], "-u")) {
            only_uniq = true;
        } else if(!strcmp(argv[i], "-c")) {
            with_count = true;
        } else {
            print_usage(argv[0], buf1);
            return 1;
        }
    }

    size_t count = 0;
    for(;;) {
        char *is_eof = fgets(cur_line, sizeof(buf1), &stdin);
        if(strcmp(cur_line, prev_line) || !is_eof) {
            if(prev_line[0] != 0 && (!only_uniq || count == 1)) {
                if(with_count) {
                    size_t num_size = itoa(count, num_buf);
                    num_buf[num_size++] = ' ';
                    write_all(1, num_buf, num_size);
                }

                write_all(1, prev_line, strlen(prev_line));
            }
            if(!is_eof)
                break;
            count = 0;
        }
        char *next_line = prev_line;
        prev_line = cur_line;
        cur_line = next_line;
        count++;
    }
}
