#include "debug.h"

#ifdef NDEBUG
#include <assert.h>

static char buf[1024] = {0};

static void ptrace_read(pid_t pid, char *dst, void *src, size_t size) {
    size_t num_words = size / sizeof(long);
    assert(num_words * sizeof(long) == size);

    long word;
    for(size_t i = 0; i < num_words; i++) {
        ptrace_d(PTRACE_PEEKTEXT, pid, src, &word);
        *(long*)dst = word;
        dst += sizeof(long);
        src += sizeof(long);
    }
}

static char* ptrace_read_str(pid_t pid, void *src, size_t size) {
    size_t new_size = (size / sizeof(long))*sizeof(long);
    if(new_size != size)
        new_size += sizeof(long);
    assert(new_size < sizeof(buf));

    ptrace_read(pid, buf, src, new_size);
    buf[size] = 0;
    return buf;
}

long _ptrace_d(const char *line, long request, pid_t pid, void *addr, void *data) {
    long res = ptrace(request, pid, addr, data);
    if(res < 0) {
        fprintf(stderr, "%s: %s\n", line, strerror(-res));
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
            printf("0x%08llx", arg);
            break;
        case ARG_STR:
            long long str_size = *(long long*)(base + fun_args_offsets[call->str_arg]);
            printf("(0x%08llx)\"", arg);

            char *str = ptrace_read_str(pid, (void*)arg, str_size);
            for(size_t i = 0; i < str_size; i++) {
                switch(str[i]) {
                case '\t': fputs("\\t", stdout);  break;
                case '\n': fputs("\\n", stdout);  break;
                case '\r': fputs("\\r", stdout);  break;
                default:   putchar(str[i]); break;
                }
            }
            putchar('"');
            break;
        }

        if(i != call->num_args - 1)
            fputs(", ", stdout);
    }
    fputs(")\n", stdout);
}

void print_regs(struct user_regs_struct *regs) {
    printf("rdi: 0x%08llx\n"
           "rsi: 0x%08llx\n"
           "rdx: 0x%08llx\n"
           "r10: 0x%08llx\n"
           "r8:  0x%08llx\n"
           "r9:  0x%08llx\n",
           regs->rdi,
           regs->rsi,
           regs->rdx,
           regs->r10,
           regs->r8,
           regs->r9);
}
#endif
