#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/prctl.h>
#include <sys/user.h>
#include <sys/ptrace.h>

#include "table.h"

static struct user_regs_struct regs;

static const uint8_t syscall_code[] = {0x0f, 0x05, 0xcc, 0x00, 0x00, 0x00, 0x00, 0x00};

static uint8_t call_code[] = {0xe8, 0x00, 0x00, 0x00, 0x00, 0xcc, 0x00, 0x00};

static inline uint32_t addr_hash(long addr) {
    uint32_t hash = 0;
    const uint8_t *key = (uint8_t*)&addr;
    for(uint32_t i = 0; i < sizeof(addr); i++) {
        hash += key[i];
        hash += (hash << 10);
        hash ^= (hash >> 6);
    }
    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);
    return hash;
}

static inline void prepare_regs(cmd_entry *entry) {
    void *args = &regs;
    long long src[6];
    for(int i = 0; i < entry->num_args; i++)
        src[i] = *(long long*)(args + entry->args_offsets[i]);
    for(int i = 0; i < entry->num_args; i++)
        *(long long*)(args + fun_args_offsets[i]) = src[i];
    if(entry->type == CMD_SYSCALL)
        regs.rax = entry->number;
}

static inline void reset_regs(cmd_entry *entry) {
    void *args = &regs;
    long long src[6];
    for(int i = 0; i < entry->num_args; i++)
        src[i] = *(long long*)(args + fun_args_offsets[i]);
    for(int i = 0; i < entry->num_args; i++)
        *(long long*)(args + entry->args_offsets[i]) = src[i];
}

static inline void call_interpreter(pid_t pid) {
    long backup = 0, code = 0;

    for(;;) {
        if(ptrace(PTRACE_CONT, pid, NULL, NULL) < 0)
            break;
        wait(NULL);

        if(ptrace(PTRACE_GETREGS, pid, NULL, &regs) < 0)
            break;
        unsigned long long rip_bak = regs.rip;

        int entry_id = addr_hash(regs.rip) % NUM_ENTRIES + (long long)regs.rax;
        cmd_entry *entry = &cmd_table[entry_id];
        if(entry->type == CMD_SYSCALL || entry->type == CMD_PROC) {
            prepare_regs(entry);

            if(entry->type == CMD_SYSCALL) {
                code = *(long*)syscall_code;
            } else {
                int32_t offset = entry->proc_func - ((void*)regs.rip + 5);
                *(int32_t*)&call_code[1] = offset;
                code = *(long*)call_code;
            }
        } else if(entry->type == CMD_REGS) {
            entry->regs_proc(&regs);
        }

        if(ptrace(PTRACE_SETREGS, pid, NULL, &regs) < 0)
            break;

        if(entry->type != CMD_REGS) {
            backup = ptrace(PTRACE_PEEKTEXT, pid, (void*)regs.rip, NULL);
            if(backup < 0 && errno)
                break;
            if(ptrace(PTRACE_POKETEXT, pid, (void*)regs.rip, (void*)code) < 0)
                break;
        }

        if(ptrace(PTRACE_CONT, pid, NULL, NULL) < 0)
            break;
        wait(NULL);

        if(entry->type == CMD_REGS)
            continue;

        if(ptrace(PTRACE_POKETEXT, pid, (void*)regs.rip, (void*)backup) < 0)
            break;
        if(ptrace(PTRACE_GETREGS, pid, NULL, &regs) < 0)
            break;
        reset_regs(entry);
        regs.rip = rip_bak;
        if(ptrace(PTRACE_SETREGS, pid, NULL, &regs) < 0)
            break;
    }
}

static inline void int3() {
    __asm__ __volatile__ ("int3");
}

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

    // children
    if(ptrace(PTRACE_TRACEME, 0, NULL, NULL) < 0)
        return EXIT_FAILURE;
    int3();

    pid_t parent = getppid();
    if(ptrace(PTRACE_SEIZE, parent, NULL, NULL) < 0)
        return EXIT_FAILURE;

    char test_buf[128];
    nano_write(1, "What's your name?\n", 18);
    int len = nano_read(0, test_buf, sizeof(test_buf));
    nano_write(1, "Hello, ", 7);
    nano_write(1, test_buf, len);

    long res = nano_someproc(20);
    // TODO: printf with nano_write inside
    printf("someproc: %li\n", res);
}
