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

#define __NR_read 0
#define __NR_write 1
#define __NR_open 2
#define __NR_close 3
#define __NR_fstat 5
#define __NR_mmap 9
#define __NR_mprotect 10
#define __NR_munmap 11
#define __NR_brk 12
#define __NR_exit 60

typedef unsigned long size_t;
typedef long ssize_t;

typedef unsigned char uint8_t;
typedef short int16_t;
typedef unsigned short uint16_t;
typedef int int32_t;
typedef unsigned int uint32_t;
typedef long int64_t;
typedef unsigned long uint64_t;

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
#define brk(addr) syscall1(__NR_brk, (unsigned long)addr)
#define exit(code) syscall1(__NR_exit, code)

#define puts(str) fputs(STDOUT, str)

static inline long syscall1(long n, long a1) {
        unsigned long ret;
        __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1) : "rcx", "r11", "memory");
        return ret;
}

static inline long syscall2(long n, long a1, long a2) {
        unsigned long ret;
        __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2)
                                                  : "rcx", "r11", "memory");
        return ret;
}

static inline long syscall3(long n, long a1, long a2, long a3) {
        unsigned long ret;
        __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2),
                                                  "d"(a3) : "rcx", "r11", "memory");
        return ret;
}

static inline long syscall4(long n, long a1, long a2, long a3, long a4) {
        unsigned long ret;
        register long r10 __asm__("r10") = a4;
        __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2),
                                                  "d"(a3), "r"(r10): "rcx", "r11", "memory");
        return ret;
}

static inline long syscall5(long n, long a1, long a2, long a3, long a4, long a5) {
        unsigned long ret;
        register long r10 __asm__("r10") = a4;
        register long r8 __asm__("r8") = a5;
        __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2),
                                                  "d"(a3), "r"(r10), "r"(r8) : "rcx", "r11", "memory");
        return ret;
}

static inline long syscall6(long n, long a1, long a2, long a3, long a4, long a5, long a6) {
        unsigned long ret;
        register long r10 __asm__("r10") = a4;
        register long r8 __asm__("r8") = a5;
        register long r9 __asm__("r9") = a6;
        __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2),
                                                  "d"(a3), "r"(r10), "r"(r8), "r"(r9) : "rcx", "r11", "memory");
        return ret;
}

static inline void memset(void *_dest, int c, size_t n) {
    char *dest = _dest;
    while(n--) *dest++ = c;
}

static inline void* memcpy(void *_dest, const void *_src, size_t n) {
    char *dest = (char*)_dest;
    const char *src = (const char*)_src;
    for(size_t i = 0; i < n; i++)
        dest[i] = src[i];
    return _dest;
}

static inline size_t strlen(const char *str) {
    size_t res = 0;
    while(*str++) res++;
    return res;
}

static inline int strcmp(const char *s1, const char *s2) {
    for(;;) {
        if(*s1 && *s2) {
            char diff = *s1++ - *s2++;
            if(diff)
                return diff;
        } else if(*s1) {
            return 1;
        } else if(*s2) {
            return -1;
        } else {
            return 0;
        }
    }
}

#endif //_LIB_H
