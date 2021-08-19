#pragma once

#include <sys/ptrace.h>

#ifdef NDEBUG
#include <sys/types.h>
#include <sys/user.h>
#include "table.h"

long _ptrace_d(const char *line, long request, pid_t pid, void *addr, void *data);
void print_nanocall_info(pid_t pid, cmd_entry *entry, struct user_regs_struct *regs);
void print_regs(struct user_regs_struct *regs);
#define Dprintf printf

#define STR2(x) #x
#define STR(x) STR2(x)
#define ptrace_d(request, pid, addr, data) \
    _ptrace_d("[" __FILE__ ":" STR(__LINE__) "] " #request, request, pid, addr, data)
#else

#define ptrace_d(request, pid, addr, data) ptrace(request, pid, addr, data)
#define print_nanocall_info(...) do { } while(0)
#define print_regs(...) do { } while(0)
#define Dprintf(...) do { } while(0)

#endif

