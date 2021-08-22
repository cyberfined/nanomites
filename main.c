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
#include "debug.h"

static struct user_regs_struct regs;

static uint8_t call_code[] = {0xe8, 0x00, 0x00, 0x00, 0x00, 0xcc, 0x00, 0x00};

static long long regs_bak[6];

static inline void prepare_regs(cmd_entry *entry) {
    void *args = &regs;
    long long src[6];
    for(int i = 0; i < entry->num_args; i++)
        src[i] = *(long long*)(args + entry->args_offsets[i]);
    for(int i = 0; i < entry->num_args; i++) {
        regs_bak[i] = *(long long*)(args + fun_args_offsets[i]);
        *(long long*)(args + fun_args_offsets[i]) = src[i];
    }
    if(entry->type == CMD_SYSCALL)
        regs.orig_rax = entry->number;
    else if(entry->type == CMD_PROC)
        regs.orig_rax = 39;
}

static inline void reset_regs(cmd_entry *entry) {
    void *args = &regs;
    for(int i = 0; i < entry->num_args; i++)
        *(long long*)(args + fun_args_offsets[i]) = regs_bak[i];
}

static inline void call_interpreter(pid_t pid) {
    long backup = 0;

    for(;;) {
        if(ptrace_d(PTRACE_SYSCALL, pid, NULL, NULL) < 0)
            break;
        wait(NULL);

        if(ptrace_d(PTRACE_GETREGS, pid, NULL, &regs) < 0)
            break;
        unsigned long long rip_bak = regs.rip;
        Dprintf("\nstarted\n");
        print_regs(&regs);

        cmd_entry *entry = nano_lookup(regs.rip);
        if(!entry) {
            if(ptrace_d(PTRACE_SYSCALL, pid, NULL, NULL) < 0)
                break;
            wait(NULL);
            continue;
        }

        if(entry->type == CMD_SYSCALL || entry->type == CMD_PROC) {
            Dprintf("\nprepared\n");
            prepare_regs(entry);
            print_regs(&regs);
            if(entry->type == CMD_SYSCALL)
                print_nanocall_info(pid, entry, &regs);
        } else if(entry->type == CMD_REGS) {
            entry->regs_proc(&regs);
        }

        if(ptrace_d(PTRACE_SETREGS, pid, NULL, &regs) < 0)
            break;

        if(ptrace_d(PTRACE_SYSCALL, pid, NULL, NULL) < 0)
            break;
        wait(NULL);

        if(entry->type == CMD_PROC) {
            int32_t offset = entry->proc_func - ((void*)regs.rip + 5);
            *(int32_t*)&call_code[1] = offset;
            backup = ptrace_d(PTRACE_PEEKTEXT, pid, (void*)regs.rip, NULL);
            if(backup < 0 && errno)
                break;
            long patch = *(long*)call_code;
            if(ptrace_d(PTRACE_POKETEXT, pid, (void*)regs.rip, (void*)patch) < 0)
                break;
            if(ptrace_d(PTRACE_CONT, pid, NULL, NULL) < 0)
                break;
            wait(NULL);
            if(ptrace_d(PTRACE_POKETEXT, pid, (void*)regs.rip, (void*)backup) < 0)
                break;
        } else if(entry->type == CMD_REGS) {
            continue;
        }

        if(ptrace_d(PTRACE_GETREGS, pid, NULL, &regs) < 0)
            break;

        Dprintf("\nreseted\n");
        reset_regs(entry);
        print_regs(&regs);
        if(entry->type == CMD_PROC)
            regs.rip = rip_bak;
        if(ptrace_d(PTRACE_SETREGS, pid, NULL, &regs) < 0)
            break;
    }
}

static inline void int3() {
    __asm__ __volatile__ ("int3");
}

static char test_buf[128];

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
    if(ptrace_d(PTRACE_TRACEME, 0, NULL, NULL) < 0)
        return EXIT_FAILURE;
    int3();

    pid_t parent = getppid();
    if(ptrace_d(PTRACE_SEIZE, parent, NULL, NULL) < 0)
        return EXIT_FAILURE;

    nano_write(1, "What's your name?\n", 18);
    int len = nano_read(0, test_buf, sizeof(test_buf));
    test_buf[len] = 0;
    nano_write(1, "Hello, ", 7);
    nano_write(1, test_buf, len);

    // TODO: printf with nano_write inside
    long res = nano_someproc(20);
    printf("someproc: %li\n", res);
}
