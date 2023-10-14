#include "libc/lib.h"

#include "table.h"

static struct user_regs_struct regs;

static uint8_t call_code[] = {0xe8, 0x00, 0x00, 0x00, 0x00, 0xcc, 0x00, 0x00};

static long long regs_bak[6];

static ssize_t write_all(int fd, char *buf, size_t size) {
    while(size != 0) {
        ssize_t wb = nano_write(fd, buf, size);
        if(wb < 0)
            return wb;
        size -= wb;
    }
    return size;
}

static void print_usage(char *prog_name, char *buf) {
    char usage_msg[] = "Usage: ";
    char flags_msg[] = " -[uc]\nCrackme v0.1.666 by cyberfined\n";

    size_t prog_name_len = strlen(prog_name);
    char *cur_char = buf;
    nano_memcpy(cur_char, usage_msg, 7);
    cur_char += 7;
    nano_memcpy(cur_char, prog_name, prog_name_len);
    cur_char += prog_name_len;
    nano_memcpy(cur_char, flags_msg, 38);
    cur_char += 38;
    write_all(STDERR, buf, cur_char - buf);
}

static void reverse(char *buf, size_t size) {
    for(size_t i = 0, j = size - 1; i < j; i++, j--) {
        char c = buf[i];
        buf[i] = buf[j];
        buf[j] = c;
    }
}

static size_t itoa(size_t num, char *buf) {
    size_t i = 0;
    do {
        buf[i++] = num % 10 + '0';
        num /= 10;
    } while(num != 0);
    reverse(buf, i);
    return i;
}

typedef struct FILE {
    int    fd;
    char   buf[4096];
    size_t offset;
    size_t size;
} FILE;

static FILE stdin = {0, {0}, 0, 0};

char* fgets(char *buf, size_t size, FILE *f) {
    size_t cpy_size = 0;

    for(;;) {
        if(f->size > 0) {
            char *newline = strchr(&f->buf[f->offset], '\n');
            if(newline) {
                cpy_size = newline - f->buf - f->offset + 1;
                if(cpy_size >= size)
                    cpy_size = size - 1;
                goto ret;
            } else if(f->offset + f->size >= sizeof(f->buf) - 2) {
                if(f->offset == 0) {
                    cpy_size = f->size;
                    if(cpy_size >= size)
                        cpy_size = size - 1;
                    goto ret;
                } else {
                    nano_memmove(f->buf, &f->buf[f->offset], f->size);
                    f->offset = 0;
                }
            }
        }

        ssize_t rb = nano_read(
            f->fd,
            &f->buf[f->offset + f->size],
            sizeof(f->buf) - 1 - f->offset - f->size
        );
        if(rb == 0) {
            return NULL;
        } else if(rb < 0) {
            return NULL;
        }
        f->size += rb;
        f->buf[f->size] = 0;

        continue;
ret:
        nano_memcpy(buf, &f->buf[f->offset], cpy_size);
        buf[cpy_size] = 0;
        f->offset += cpy_size;
        f->size -= cpy_size;
        if(f->size == 0)
            f->offset = 0;
        break;
    }

    return buf;
}

// Interpreter

#include "table.inc"

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
        ptrace(PTRACE_SYSCALL, pid, NULL, NULL);
        wait_or_die();

        ptrace(PTRACE_GETREGS, pid, NULL, &regs);
        unsigned long long rip_bak = regs.rip;

        cmd_entry *entry = nano_lookup(regs.rip);
        if(!entry) {
            ptrace(PTRACE_SYSCALL, pid, NULL, NULL);
            wait_or_die();
            continue;
        }

        if(entry->type == CMD_SYSCALL || entry->type == CMD_PROC) {
            prepare_regs(entry);
        } else if(entry->type == CMD_REGS) {
            entry->regs_proc(&regs);
        }

        ptrace(PTRACE_SETREGS, pid, NULL, &regs);
        ptrace(PTRACE_SYSCALL, pid, NULL, NULL);
        wait_or_die();

        if(entry->type == CMD_PROC) {
            int32_t offset = entry->proc_func - ((void*)regs.rip + 5);
            *(int32_t*)&call_code[1] = offset;
            ptrace(PTRACE_PEEKTEXT, pid, (void*)regs.rip, &backup);
            long patch = *(long*)call_code;
            ptrace(PTRACE_POKETEXT, pid, (void*)regs.rip, (void*)patch);
            ptrace(PTRACE_CONT, pid, NULL, NULL);
            wait_or_die();
            ptrace(PTRACE_POKETEXT, pid, (void*)regs.rip, (void*)backup);
        } else if(entry->type == CMD_REGS) {
            continue;
        }

        ptrace(PTRACE_GETREGS, pid, NULL, &regs);

        reset_regs(entry);
        if(entry->type == CMD_PROC)
            regs.rip = rip_bak;
        ptrace(PTRACE_SETREGS, pid, NULL, &regs);
    }
}

__attribute__((always_inline))
static inline void int3() {
    __asm__ __volatile__ ("int3");
}

typedef struct {
    int      offset;
    uint16_t key;
} pass_info_t;

static bool check_password(pass_info_t *pass_info, size_t count) {
    uint16_t password[8] = {42824,42175,27030,55028,31997,4578,9264};

    if(pass_info->offset == 7) {
        struct timespec tv;
        nano_clock_gettime(CLOCK_REALTIME, &tv);
        password[7] = (tv.tv_sec / (24 * 3600) % 229) ^ 40704;
    }

    if(pass_info->offset < 8) {
        if((password[pass_info->offset] ^ pass_info->key) == count) {
            pass_info->key *= (pass_info->offset + 10) * 5;
            pass_info->offset++;
            return true;
        }
    } else {
        return true;
    }
    return false;
}

int main(int argc, char **argv) {
    // killer256
    char greetings[] = "Crackme v0.1.666 by cyberfined\nReading from stdin\n";
    char success_msg[] = "Congratulations, dear hacker!\n";
    char fail_msg[] = "Password is wrong!\n";

    bool only_uniq = false, with_count = false;
    char num_buf[16], buf1[4096], buf2[4096] = {0};
    char *cur_line = buf1, *prev_line = buf2;

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
    if(ptrace(PTRACE_TRACEME, 0, NULL, NULL) < 0)
        exit(EXIT_FAILURE);
    int3();

    pid_t parent = getppid();
    if(ptrace(PTRACE_SEIZE, parent, NULL, NULL) < 0)
        exit(EXIT_FAILURE);

    if(argc > 3) {
        print_usage(argv[0], buf1);
        nano_exit(EXIT_FAILURE);
    } else if(argc == 1) {
        nano_write(2, greetings, sizeof(greetings) - 1);
    }

    for(int i = 1; i < argc; i++) {
        if(!strcmp(argv[i], "-u")) {
            only_uniq = true;
        } else if(!strcmp(argv[i], "-c")) {
            with_count = true;
        } else {
            print_usage(argv[0], buf1);
            nano_exit(EXIT_FAILURE);
        }
    }

    pass_info_t pass_info = {
        .key    = 42787,
        .offset = 0,
    };

    bool is_auth = true;
    size_t count = 0;
    for(;;) {
        char *is_eof = fgets(cur_line, sizeof(buf1), &stdin);
        if(strcmp(cur_line, prev_line) || !is_eof) {
            if(prev_line[0] != 0 && (!only_uniq || count == 1)) {
                if(with_count) {
                    size_t num_size = nano_itoa(count, num_buf);
                    num_buf[num_size++] = ' ';
                    write_all(1, num_buf, num_size);
                }

                write_all(1, prev_line, strlen(prev_line));
                is_auth = is_auth && check_password(&pass_info, count);
            }

            if(!is_eof)
                break;
            count = 0;
        }
        char *next_line = prev_line;
        prev_line = cur_line;
        cur_line = next_line;
        count++;
    }

    if(is_auth && pass_info.offset == 8) {
        nano_write(1, success_msg, sizeof(success_msg) - 1);
    } else {
        nano_write(1, fail_msg, sizeof(fail_msg) - 1);
    }

    nano_exit(EXIT_SUCCESS);
}
