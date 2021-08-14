#pragma once

#include <stddef.h>
#ifdef __unix__
typedef struct {
    int    fd;
    size_t size;
    void   *data;
} FileMap;
#elif defined(_WIN32)
#include <windows.h>
typedef struct {
    HANDLE fd;
    HANDLE mapd;
    size_t size;
    LPVOID data;
} FileMap;
#else
#error "Unsupported os: please define file_map.h and file_map.c yourself"
#endif

FileMap* FileMapCreate(const char *filename);
void FileMapDestroy(FileMap *m);
void* FileMapGetPointer(FileMap *m);
size_t FileMapGetSize(FileMap *m);
