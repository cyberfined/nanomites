#include "table.h"
#include <stdio.h>

static long someproc(long arg) {
    return arg * 20;
}

static void someregs(struct user_regs_struct *regs) {
}

#include "table.inc"
