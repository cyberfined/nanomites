#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/user.h>

#include "table.h"

#define MOV_SIZE         7
#define ARG_PATCH_OFFSET 3
#define NUM_SYSCALLS     335
#define BACKUP_SIZE      32

#define SYS_READ  1
#define SYS_WRITE 2
#define SYS_EXIT  61

struct syscall_t;

typedef void(*patch_func)(struct syscall_t*,struct user_regs_struct*);

typedef struct syscall_t {
    char       *code;
    size_t     code_size;
    patch_func patch;
} syscall_t;

static inline void int3() {
    __asm__ __volatile__ ("int3");
}

static char backup[BACKUP_SIZE];

static void patch1(syscall_t *call, struct user_regs_struct *regs) {
    *(unsigned int*)&call->code[0*MOV_SIZE + ARG_PATCH_OFFSET] = regs->rax;
    *(unsigned int*)&call->code[1*MOV_SIZE + ARG_PATCH_OFFSET] = regs->rdi;
}

static void patch3(syscall_t *call, struct user_regs_struct *regs) {
    *(unsigned int*)&call->code[0*MOV_SIZE + ARG_PATCH_OFFSET] = regs->rax;
    *(unsigned int*)&call->code[1*MOV_SIZE + ARG_PATCH_OFFSET] = regs->rdi;
    *(unsigned int*)&call->code[2*MOV_SIZE + ARG_PATCH_OFFSET] = regs->rsi;
    *(unsigned int*)&call->code[3*MOV_SIZE + ARG_PATCH_OFFSET] = regs->rdx;
}

/*
 * mov $0, %rax
 * mov $0, %rdi
 * syscall
 * int3
 */
static char sys1_code[] = {0x48,0xc7,0xc0,0x00,0x00,0x00,0x00,
                           0x48,0xc7,0xc7,0x00,0x00,0x00,0x00,
                           0x0f,0x05,0xcc,
                           0x00,0x00,0x00,0x00,0x00,0x00,0x00};

/*
 * mov $0, %rax
 * mov $0, %rdi
 * mov $0, %rsi
 * mov $0, %rdx
 * syscall
 * int3
 */
static char sys3_code[] = {0x48,0xc7,0xc0,0x00,0x00,0x00,0x00,
                           0x48,0xc7,0xc7,0x00,0x00,0x00,0x00,
                           0x48,0xc7,0xc6,0x00,0x00,0x00,0x00,
                           0x48,0xc7,0xc2,0x00,0x00,0x00,0x00,
                           0x0f,0x05,0xcc,0x00};

static syscall_t sys1 = {
    .code      = sys1_code,
    .code_size = sizeof(sys1_code),
    .patch     = patch1,
};

static syscall_t sys3 = {
    .code      = sys3_code,
    .code_size = sizeof(sys3_code),
    .patch     = patch3,
};

static syscall_t *syscalls[NUM_SYSCALLS] = {
    [SYS_READ]  = &sys3,
    [SYS_WRITE] = &sys3,
    [SYS_EXIT]  = &sys1,
};

static struct user_regs_struct regs;

static inline int ptrace_read(pid_t pid, char *dest, void *src, size_t size) {
    const void *src_end = src + size;
    for(; src < src_end; src += sizeof(long)) {
        long word = ptrace(PTRACE_PEEKTEXT, pid, src, NULL);
        if(word < 0 && errno)
            return -1;
        *(long*)dest = word;
        dest += sizeof(long);
    }
    return 0;
}

static inline int ptrace_write(pid_t pid, void *dest, char *src, size_t size) {
    const char *src_end = src + size;
    for(; src < src_end; src += sizeof(long)) {
        if(ptrace(PTRACE_POKETEXT, pid, dest, *(long*)src) < 0)
            return -1;
        dest += sizeof(long);
    }
    return 0;
}

static inline void call_interpreter(pid_t pid) {
    for(;;) {
        if(ptrace(PTRACE_CONT, pid, NULL, NULL) < 0)
            break;
        wait(NULL);

        if(ptrace(PTRACE_GETREGS, pid, NULL, &regs) < 0)
            break;
        unsigned long long rip_bak = regs.rip;

        syscall_t *call = syscalls[regs.rax];
        regs.rax--;
        call->patch(call, &regs);

        if(ptrace_read(pid, backup, (void*)regs.rip, call->code_size) < 0)
            break;
        if(ptrace_write(pid, (void*)regs.rip, call->code, call->code_size) < 0)
            break;

        if(ptrace(PTRACE_CONT, pid, NULL, NULL) < 0)
            break;
        wait(NULL);
        if(ptrace_write(pid, (void*)regs.rip, backup, call->code_size) < 0)
            break;
        if(ptrace(PTRACE_GETREGS, pid, NULL, &regs) < 0)
            break;
        regs.rip = rip_bak;
        if(ptrace(PTRACE_SETREGS, pid, NULL, &regs) < 0)
            break;
    }
}

__attribute__((__noinline__,__noclone__))
void somefunc() {
    puts("lilka");
}

extern char lab1[];
extern char lab2[];

int main() {
    pid_t pid = fork();
    if(pid > 0) {
        // parent
        wait(NULL);

        sigset_t sigmask;
        sigemptyset(&sigmask);
        sigaddset(&sigmask, SIGCHLD);
        if(sigprocmask(SIG_BLOCK, &sigmask, NULL) < 0)
            return EXIT_FAILURE;

        if(prctl(PR_SET_PTRACER, pid, 0, 0, 0) < 0)
            return EXIT_FAILURE;
        call_interpreter(pid);
        return EXIT_SUCCESS;
    } else if(pid < 0) {
        return EXIT_FAILURE;
    }

    nano_write(1, "Hello World\n", 12);
    nano_someproc(11);
    somefunc();

    // children
    if(ptrace(PTRACE_TRACEME, 0, NULL, NULL) < 0)
        return EXIT_FAILURE;
    int3();

    pid_t parent = getppid();
    if(ptrace(PTRACE_SEIZE, parent, NULL, NULL) < 0)
        return EXIT_FAILURE;
}
