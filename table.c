#include "table.h"

static long someproc(long arg) {
    return 1;
}

static bool someregs(struct user_regs_struct *regs) {
    return false;
}

#include "table.inc"
