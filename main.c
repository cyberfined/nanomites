#include "libc/lib.h"

#include "table.h"
#include "debug.h"

static struct user_regs_struct regs;

static uint8_t call_code[] = {0xe8, 0x00, 0x00, 0x00, 0x00, 0xcc, 0x00, 0x00};

static long long regs_bak[6];

__attribute__((always_inline))
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

__attribute__((always_inline))
static inline void reset_regs(cmd_entry *entry) {
    void *args = &regs;
    for(int i = 0; i < entry->num_args; i++)
        *(long long*)(args + fun_args_offsets[i]) = regs_bak[i];
}

__attribute__((always_inline))
static inline void wait_or_die(void) {
    int status;
    wait(&status);
    if(WIFEXITED(status))
        exit(EXIT_SUCCESS);
}

__attribute__((always_inline))
static inline void call_interpreter(pid_t pid) {
    long backup = 0;

    for(;;) {
        ptrace_d(PTRACE_SYSCALL, pid, NULL, NULL);
        wait_or_die();

        ptrace_d(PTRACE_GETREGS, pid, NULL, &regs);
        unsigned long long rip_bak = regs.rip;
        Dprintf("\nstarted\n");
        print_regs(&regs);

        cmd_entry *entry = nano_lookup(regs.rip);
        if(!entry) {
            ptrace_d(PTRACE_SYSCALL, pid, NULL, NULL);
            wait_or_die();
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

        ptrace_d(PTRACE_SETREGS, pid, NULL, &regs);
        ptrace_d(PTRACE_SYSCALL, pid, NULL, NULL);
        wait_or_die();

        if(entry->type == CMD_PROC) {
            int32_t offset = entry->proc_func - ((void*)regs.rip + 5);
            *(int32_t*)&call_code[1] = offset;
            ptrace_d(PTRACE_PEEKTEXT, pid, (void*)regs.rip, &backup);
            long patch = *(long*)call_code;
            ptrace_d(PTRACE_POKETEXT, pid, (void*)regs.rip, (void*)patch);
            ptrace_d(PTRACE_CONT, pid, NULL, NULL);
            wait_or_die();
            ptrace_d(PTRACE_POKETEXT, pid, (void*)regs.rip, (void*)backup);
        } else if(entry->type == CMD_REGS) {
            continue;
        }

        ptrace_d(PTRACE_GETREGS, pid, NULL, &regs);

        Dprintf("\nreseted\n");
        reset_regs(entry);
        print_regs(&regs);
        if(entry->type == CMD_PROC)
            regs.rip = rip_bak;
        ptrace_d(PTRACE_SETREGS, pid, NULL, &regs);
    }
}

__attribute__((always_inline))
static inline void int3() {
    __asm__ __volatile__ ("int3");
}

static char test_buf[128];

void _start(void) {
    pid_t pid = fork();
    if(pid > 0) {
        // parent
        wait_or_die();

        sigset_t sigmask = 1 << (SIGCHLD - 1);
        if(sigprocmask(SIG_BLOCK, &sigmask, NULL) < 0)
            exit(EXIT_FAILURE);

        if(prctl(PR_SET_PTRACER, pid, 0, 0, 0) < 0)
            exit(EXIT_FAILURE);
        call_interpreter(pid);
        exit(EXIT_SUCCESS);
    } else if(pid < 0) {
        exit(EXIT_FAILURE);
    }

    // children
    if(ptrace_d(PTRACE_TRACEME, 0, NULL, NULL) < 0)
        exit(EXIT_FAILURE);
    int3();

    pid_t parent = getppid();
    if(ptrace_d(PTRACE_SEIZE, parent, NULL, NULL) < 0)
        exit(EXIT_FAILURE);

    nano_write(1, "What's your name?\n", 18);
    int len = nano_read(0, test_buf, sizeof(test_buf));
    test_buf[len] = 0;
    nano_write(1, "Hello, ", 7);
    nano_write(1, test_buf, len);

    // TODO: printf with nano_write inside
    long res = nano_someproc(20);
    printf("someproc: %ld\n", res);
    nano_exit(EXIT_FAILURE);
}
