#include "lib.h"

int putchar(int _c) {
    unsigned char c = _c;
    return write(1, &c, 1);
}
