#include "debug.h"

#ifdef NDEBUG
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <stdbool.h>

static char buf[1024] = {0};

static bool ptrace_read(pid_t pid, char *dst, void *src, size_t size) {
    size_t num_words = size / sizeof(long);
    assert(num_words * sizeof(long) == size);

    for(size_t i = 0; i < num_words; i++) {
        long word = ptrace_d(PTRACE_PEEKTEXT, pid, src, NULL);
        if(word < 0 && errno)
            return false;
        *(long*)dst = word;
        dst += sizeof(long);
        src += sizeof(long);
    }

    return true;
}

static char* ptrace_read_str(pid_t pid, void *src, size_t size) {
    size_t new_size = (size / sizeof(long))*sizeof(long);
    if(new_size != size)
        new_size += sizeof(long);
    assert(new_size < sizeof(buf));

    if(!ptrace_read(pid, buf, src, new_size))
        return NULL;
    buf[size] = 0;
    return buf;
}

long _ptrace_d(const char *line, long request, pid_t pid, void *addr, void *data) {
    long res = ptrace(request, pid, addr, data);
    if(res < 0 && errno) {
        fprintf(stderr, "%s: %s\n", line, strerror(errno));
        exit(EXIT_FAILURE);
    }
    return res;
}

enum syscall_arg {
    ARG_INT,
    ARG_ADDR,
    ARG_STR
};

typedef struct {
    char   *name;
    size_t num_args;
    int    str_arg;
    int    args[6];
} syscall_t;

static syscall_t syscalls[] = {
    [0] = (syscall_t) {
        .name     = "read",
        .num_args = 3,
        .args = {ARG_INT, ARG_ADDR, ARG_INT},
    },
    [1] = (syscall_t) {
        .name     = "write",
        .num_args = 3,
        .args     = {ARG_INT, ARG_STR, ARG_INT},
        .str_arg  = 2,
    },
    [60] = (syscall_t) {
        .name     = "exit",
        .num_args = 1,
        .args     = {ARG_INT},
    },
};

void print_nanocall_info(pid_t pid, cmd_entry *entry, struct user_regs_struct *regs) {
    syscall_t *call = &syscalls[entry->number];
    void *base = regs;

    printf("%s(", call->name);
    for(size_t i = 0; i < call->num_args; i++) {
        long long arg = *(long long*)(base + fun_args_offsets[i]);
        switch(call->args[i]) {
        case ARG_INT:
            printf("%lld", arg);
            break;
        case ARG_ADDR:
            printf("%p", (void*)arg);
            break;
        case ARG_STR:
            long long str_size = *(long long*)(base + fun_args_offsets[call->str_arg]);
            printf("(%p)", (void*)arg);
            fputc('"', stdout);

            char *str = ptrace_read_str(pid, (void*)arg, str_size);
            assert(str != NULL);
            for(size_t i = 0; i < str_size; i++) {
                switch(str[i]) {
                case '\t': fputs("\\t", stdout);  break;
                case '\n': fputs("\\n", stdout);  break;
                case '\r': fputs("\\r", stdout);  break;
                default:   fputc(str[i], stdout); break;
                }
            }
            fputc('"', stdout);
            break;
        }

        if(i != call->num_args - 1)
            fputs(", ", stdout);
    }
    fputs(")\n", stdout);
}

void print_regs(struct user_regs_struct *regs) {
    printf("rdi: 0x%08lx\n"
           "rsi: 0x%08lx\n"
           "rdx: 0x%08lx\n"
           "r10: 0x%08lx\n"
           "r8:  0x%08lx\n"
           "r9:  0x%08lx\n",
           regs->rdi,
           regs->rsi,
           regs->rdx,
           regs->r10,
           regs->r8,
           regs->r9);
}
#endif
