#include "file_map.h"

#ifdef __unix__
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
FileMap* FileMapCreate(const char *filename) {
    FileMap *m = malloc(sizeof(FileMap));
    if(!m) {
        perror("FileMapCreate");
        goto error;
    }

    m->fd = open(filename, O_RDONLY);
    if(m->fd < 0)
        goto error;

    struct stat info;
    if(fstat(m->fd, &info) < 0)
        goto error;
    m->size = info.st_size;

    m->data = mmap(NULL, info.st_size, PROT_READ | PROT_WRITE, MAP_PRIVATE, m->fd, 0);
    if(m->data == MAP_FAILED)
        goto error;
    
    return m;
error:
    perror("FileMapCreate");
    if(m) {
        if(m->fd >= 0) close(m->fd);
        free(m);
    }
    return NULL;
}

void FileMapDestroy(FileMap *m) {
    munmap(m->data, m->size);
    close(m->fd);
    free(m);
}

void* FileMapGetPointer(FileMap *m) {
    return m->data;
}

size_t FileMapGetSize(FileMap *m) {
    return m->size;
}
#elif defined(_WIN32)
#include <windows.h>
#include <wchar.h>
FileMap* FileMapCreate(const char *filename) {
    LPVOID errBuf;
    m->mapd = NULL;

    FileMap *m = malloc(sizeof(FileMap));
    if(!m)
        goto error;

    m->fd = CreateFile(filename, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if(m->fd == INVALID_HANDLE_VALUE)
        goto error;

    m->size = GetFileSize(m->fd, NULL);
    if(m->size == INVALID_FILE_SIZE)
        goto error;

    m->mapd = CreateFileMapping(m->fd, NULL, PAGE_READONLY, 0, 0, NULL);
    if(m->mapd == NULL)
        goto error;

    m->data = MapViewOfFile(m->mapd, FILE_MAP_READ, 0, 0, m->size); 
    if(m->data == NULL)
        goto error;

    return 0;
error:
    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                  FORMAT_MESSAGE_FROM_SYSTEM     |
                  FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL,
                  GetLastError(),
                  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                  (LPTSTR)&errBuf,
                  0, NULL);
    wprintf(L"createFileMap: %s\n", errBuf);
    LocalFree(errBuf);
    if(m) {
        if(m->fd != INVALID_HANDLE_VALUE) CloseHandle(m->fd);
        if(m->mapd != NULL) CloseHandle(m->mapd);
        free(m);
    }
    return -1;
}

void FileMapDestroy(FileMap *m) {
    UnmapViewOfFile(m->data);
    CloseHandle(m->mapd);
    CloseHandle(m->fd);
    free(m);
}

void* FileMapGetPointer(FileMap *m) {
    return m->data;
}

size_t FileMapGetSize(FileMap *m) {
    return m->size;
}
#endif
