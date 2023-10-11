#include "lib.h"

static FILE _stdout = {1, {0}, {0}, 0, 0};
FILE *stdout = &_stdout;

static FILE _stderr = {2, {0}, {0}, 0, 0};
FILE *stderr = &_stderr;

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

typedef enum {
    INT,
    LONG,
    LONG_LONG,
} arg_len_t;

static inline void reverse(char *buf, size_t size) {
    for(size_t i = 0, j = size - 1; i < j; i++, j--) {
        char c = buf[i];
        buf[i] = buf[j];
        buf[j] = c;
    }
}

static inline size_t utoa(char *buf, size_t num, int base) {
    static char digits[16] = "0123456789abcdef";

    size_t size = 0;
    do {
        buf[size++] = digits[num % base];
        num /= base;
    } while(num != 0);
    reverse(buf, size);
    return size;
}

static inline size_t itoa(char *buf, ssize_t num) {
    bool is_signed;
    size_t size = 0;
    if(num < 0) {
        num = -num;
        buf[size++] = '-';
        is_signed = true;
    } else {
        is_signed = false;
    }

    do {
        buf[size++] = num % 10 + '0';
        num /= 10;
    } while(num != 0);

    size_t reverse_size;
    if(is_signed) {
        reverse_size = size - 1;
        buf++;
    } else {
        reverse_size = size;
    }
    reverse(buf, reverse_size);

    return size;
}

static inline size_t atoi(const char *str, int *_res) {
    static int powers[] = {1, 10, 100, 1000, 10000, 100000, 1000000, 
        10000000, 100000000, 1000000000};

    size_t size = 0;
    for(;;) {
        if(str[size] < '0' || str[size] > '9')
            break;
        size++;
    }

    int res = 0;
    for(size_t i = 0; i < size; i++)
        res += (str[i] - '0') * powers[size - i - 1];

    *_res = res;
    return size;
}

int vfprintf(FILE *f, const char *format, va_list ap) {
    int written = 0;
    bool is_format = false;
    size_t wbuf_size = 0;
    arg_len_t arg_len = INT;
    char num_buf[24];
    size_t unum;
    ssize_t snum;
    size_t to_write = 0;
    size_t copy_size;
    const char *src = format;
    int base;
    int padding_size = 0;
    size_t padding_chars;
    char padding_char = ' ';

    while(*format != 0) {
        if(*format == '%' || is_format) {
            if(!is_format)
                format++;
            is_format = true;
            switch(*format) {
            case 'l':
                format++;
                if(*format == 'l') {
                    format++;
                    arg_len = LONG_LONG;
                } else {
                    arg_len = LONG;
                }
                continue;
            case 'd':
                if(arg_len == INT)
                    snum = va_arg(ap, int);
                else if(arg_len == LONG)
                    snum = va_arg(ap, long);
                else
                    snum = va_arg(ap, long long);
                to_write = itoa(num_buf, snum);
                src = num_buf;
                break;
            case 'x':
            case 'u':
                base = *format == 'x' ? 16 : 10;
                if(arg_len == INT)
                    unum = va_arg(ap, unsigned int);
                else if(arg_len == LONG)
                    unum = va_arg(ap, unsigned long);
                else
                    unum = va_arg(ap, unsigned long long);
                to_write = utoa(num_buf, unum, base);
                src = num_buf;
                break;
            case 's':
                src = va_arg(ap, char*);
                to_write = strlen(src);
                break;
            case '%':
                is_format = false;
                break;
            case '0':
                padding_chars = atoi(format+1, &padding_size);
                if(padding_chars == 0) {
                    is_format = false;
                    break;
                }

                padding_char = '0';
                format += padding_chars + 1;
                continue;
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                padding_char = ' ';
                padding_chars = atoi(format, &padding_size);
                format += padding_chars;
                continue;
            }
            arg_len = INT;
        }

        if(is_format) {
            if(padding_size >= to_write)
                padding_size -= to_write;

            if(wbuf_size + padding_size < sizeof(f->wbuf)) {
                memset(&f->wbuf[wbuf_size], padding_char, padding_size);
                wbuf_size += padding_size;
            } else {
                copy_size = sizeof(f->wbuf) - wbuf_size;
                memset(&f->wbuf[wbuf_size], padding_char, padding_size);
                wbuf_size = padding_size - copy_size;
                if(!write_all(f->fd, f->wbuf, sizeof(f->wbuf)))
                    return -1;
                memset(f->wbuf, padding_char, wbuf_size);
            }

            if(wbuf_size + to_write < sizeof(f->wbuf)) {
                memcpy(&f->wbuf[wbuf_size], src, to_write);
                wbuf_size += to_write;
            } else {
                copy_size = sizeof(f->wbuf) - wbuf_size;
                memcpy(&f->wbuf[wbuf_size], src, copy_size);
                wbuf_size = to_write - copy_size;
                if(!write_all(f->fd, f->wbuf, sizeof(f->wbuf)))
                    return -1;
                memcpy(f->wbuf, &src[copy_size], wbuf_size);
            }
            written += to_write;
            is_format = false;
            padding_size = 0;
        } else {
            if(wbuf_size + 1 < sizeof(f->wbuf)) {
                f->wbuf[wbuf_size++] = *format;
            } else {
                if(!write_all(f->fd, f->wbuf, sizeof(f->wbuf)))
                    return -1;
                f->wbuf[0] = *format;
                wbuf_size = 1;
            }
            written++;
        }

        format++;
    }

    if(wbuf_size != 0) {
        if(!write_all(f->fd, f->wbuf, wbuf_size))
            return -1;
    }

    return written;
}

__attribute__ ((format (printf, 2, 3)))
int fprintf(FILE *f, const char *format, ...) {
    va_list ap;
    va_start(ap, format);
    int written = vfprintf(f, format, ap);
    va_end(ap);
    return written;
}

__attribute__ ((format (printf, 1, 2)))
int printf(const char *format, ...) {
    va_list ap;
    va_start(ap, format);
    int written = vfprintf(stdout, format, ap);
    va_end(ap);
    return written;
}
