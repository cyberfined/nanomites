#ifndef _LIB_H
#define _LIB_H

#define NULL ((void*)0)

#define O_RDONLY 00
#define O_WRONLY 01
#define O_RDWR   02

#define MAP_SHARED    0x01
#define MAP_PRIVATE   0x02
#define MAP_FIXED     0x10
#define MAP_ANONYMOUS 0x20
#define MAP_FAILED ((void*)-1)

#define STDIN  0
#define STDOUT 1
#define STDERR 2

#define PROT_READ       0x1
#define PROT_WRITE      0x2
#define PROT_EXEC       0x4
#define PROT_SEM        0x8
#define PROT_NONE       0x0
#define PROT_GROWSDOWN  0x01000000
#define PROT_GROWSUP    0x02000000

#define PR_SET_PTRACER   0x59616d61
#define PTRACE_TRACEME   0
#define PTRACE_PEEKTEXT  1
#define PTRACE_POKETEXT  4
#define PTRACE_CONT      7
#define PTRACE_GETREGS   12
#define PTRACE_SETREGS   13
#define PTRACE_SYSCALL   24
#define PTRACE_SYSEMU    31
#define PTRACE_SEIZE     0x4206

#define	EXIT_FAILURE 1
#define	EXIT_SUCCESS 0

#define SIG_BLOCK 0
#define SIGCHLD   17
#define WIFEXITED(s) (!((s) & 0x7f))

#define __NR_read 0
#define __NR_write 1
#define __NR_open 2
#define __NR_close 3
#define __NR_fstat 5
#define __NR_mmap 9
#define __NR_mprotect 10
#define __NR_munmap 11
#define __NR_brk 12
#define __NR_sigprocmask 14
#define __NR_fork 57
#define __NR_exit 60
#define __NR_wait4 61
#define __NR_ptrace 101
#define __NR_getppid 110
#define __NR_prctl 157

typedef unsigned long size_t;
typedef long ssize_t;

typedef unsigned char uint8_t;
typedef short int16_t;
typedef unsigned short uint16_t;
typedef int int32_t;
typedef unsigned int uint32_t;
typedef long int64_t;
typedef unsigned long uint64_t;
typedef long pid_t;
typedef unsigned long sigset_t;

typedef signed char bool;
#define false 0
#define true 1

struct timespec {
    long tv_sec;
    long tv_nsec;
};

struct stat {
    uint64_t st_dev;
    uint64_t st_ino;
    uint64_t st_nlink;
    int      st_mode;
    int      st_uid;
    int      st_gid;
    unsigned int _pad0;
    uint64_t st_rdev;
    uint64_t st_size;
    uint64_t st_blksize;
    uint64_t st_blocks;

    struct timespec st_atim;
    struct timespec st_mtim;
    struct timespec st_ctim;
    long __unused[3];
};

#define read(fd, buf, count) syscall3(__NR_read, fd, (long)buf, count)
#define write(fd, buf, count) syscall3(__NR_write, fd, (long)buf, count)
#define open(filename, flags, mode) syscall3(__NR_open, (long)filename, flags, mode)
#define close(fd) syscall1(__NR_close, fd)
#define fstat(fd, stat) syscall2(__NR_fstat, fd, (long)stat)
#define mmap(addr, len, prot, flags, fd, off) (void*)syscall6(__NR_mmap, (long)addr, len, prot, flags, fd, off)
#define mprotect(start, len, prot) syscall3(__NR_mprotect, (long)start, len, prot)
#define munmap(addr, len) syscall2(__NR_munmap, (long)addr, len)
#define sigprocmask(how, set, oset) \
    syscall4(__NR_sigprocmask, how, (unsigned long)set, (unsigned long)oset, sizeof(sigset_t))
#define fork() syscall0(__NR_fork)
#define brk(addr) syscall1(__NR_brk, (unsigned long)addr)
#define exit(code) syscall1(__NR_exit, code)
#define wait(wstatus) \
    syscall4(__NR_wait4, -1, (unsigned long)wstatus, 0, (unsigned long)NULL)
#define ptrace(req, pid, addr, data) \
    syscall4(__NR_ptrace, req, pid, (unsigned long)addr, (unsigned long)data)
#define getppid() syscall0(__NR_getppid)
#define prctl(option, arg2, arg3, arg4, arg5) \
    syscall5(__NR_prctl, option, arg2, arg3, arg5, arg5)

__attribute__((always_inline))
static inline long syscall0(long n) {
    unsigned long ret;
    __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n) : "rcx", "r11", "memory");
    return ret;
}


__attribute__((always_inline))
static inline long syscall1(long n, long a1) {
    unsigned long ret;
    __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1) : "rcx", "r11", "memory");
    return ret;
}

__attribute__((always_inline))
static inline long syscall2(long n, long a1, long a2) {
    unsigned long ret;
    __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2)
                                              : "rcx", "r11", "memory");
    return ret;
}

__attribute__((always_inline))
static inline long syscall3(long n, long a1, long a2, long a3) {
    unsigned long ret;
    __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2),
                                              "d"(a3) : "rcx", "r11", "memory");
    return ret;
}

__attribute__((always_inline))
static inline long syscall4(long n, long a1, long a2, long a3, long a4) {
    unsigned long ret;
    register long r10 __asm__("r10") = a4;
    __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2),
                                              "d"(a3), "r"(r10): "rcx", "r11", "memory");
    return ret;
}

__attribute__((always_inline))
static inline long syscall5(long n, long a1, long a2, long a3, long a4, long a5) {
    unsigned long ret;
    register long r10 __asm__("r10") = a4;
    register long r8 __asm__("r8") = a5;
    __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2),
                                              "d"(a3), "r"(r10), "r"(r8) : "rcx", "r11", "memory");
    return ret;
}

__attribute__((always_inline))
static inline long syscall6(long n, long a1, long a2, long a3, long a4, long a5, long a6) {
    unsigned long ret;
    register long r10 __asm__("r10") = a4;
    register long r8 __asm__("r8") = a5;
    register long r9 __asm__("r9") = a6;
    __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2),
                                              "d"(a3), "r"(r10), "r"(r8), "r"(r9) : "rcx", "r11", "memory");
    return ret;
}

size_t strlen(const char *s);
int strcmp(const char *l, const char *r);
void* memcpy(void *dest, const void *src, size_t n);
void* memmove(void *dest, const void *src, size_t n);
char* strchr(const char *s, int c);

typedef struct FILE {
    int    fd;
    char   buf[4096];
    size_t offset;
    size_t size;
} FILE;
extern FILE *stdin;
char* fgets(char *buf, size_t size, FILE *f);

#endif //_LIB_H
