#include "lib.h"

#include "table.inc"

//#define INLINE __attribute__((always_inline))
#define INLINE

typedef struct {
    long long addr;
    uint32_t  size;
    uint8_t   data[16];
} func_node;

__attribute__((section(".data")))
func_node funcs_table[FUNCS_TABLE_SIZE];

typedef void* (generic_func)(void*,void*,void*,void*,void*,void*);

void _start(void);

generic_func *prev_caller = (generic_func*)_start;

INLINE
static inline uint32_t jenkins_hash_func(long long addr) {
    size_t i = 0;
    uint32_t hash = 0;
    uint8_t *key = (uint8_t*)&addr;
    while (i != sizeof(addr)) {
        hash += key[i++];
        hash += hash << 10;
        hash ^= hash >> 6;
    }
    hash += hash << 3;
    hash ^= hash >> 11;
    hash += hash << 15;
    return hash;
}

INLINE
static inline func_node* func_lookup(long long addr) {
    uint32_t hash = jenkins_hash_func(addr);
    int cur = hash % FUNCS_TABLE_SIZE;
    int step = hash % (FUNCS_TABLE_SIZE - 2) + 1;
    //int cur = hash % fucking_size;
    //int step = hash % (fucking_size - 2) + 1;
    int nstep = step;

    for(size_t i = 0; i < FUNCS_TABLE_SIZE; i++) {
        func_node *cur_node = &funcs_table[cur];
        if(!cur_node->addr)
            break;
        if(cur_node->addr == addr)
            return cur_node;
        cur = (hash + nstep) % FUNCS_TABLE_SIZE;
        nstep += step;
    }

    return NULL;
}

INLINE
static inline void crypt_function_common(func_node *func) {
    void *data = (void*)func->addr;

    uint64_t key = *(uint64_t*)func->data;
    size_t i, j;
    for(i = 0, j = func->size / sizeof(uint64_t);
        j != 0;
        i += sizeof(uint64_t), j--)
    {
        *(uint64_t*)(data + i) ^= key;
    }

    for(j = 0; i < func->size; i++, j++)
        *(uint8_t*)(data + i) ^= (key >> (j * 8)) & 0xff;
}

INLINE
static inline void encrypt_function(func_node *func) {
    crypt_function_common(func);

    void *data = (void*)func->addr;
    uint64_t tmp;
    tmp = *(uint64_t*)data;
    *(uint32_t*)data = *(uint32_t*)&func->data[8];
    *(uint8_t*)(data + 4) = func->data[12];
    *(uint64_t*)&func->data[8] = tmp;
}

INLINE
static inline void decrypt_function(func_node *func) {
    void *data = (void*)func->addr;

    uint64_t tmp;
    tmp = *(uint64_t*)data;
    *(uint32_t*)data = *(uint32_t*)&func->data[8];
    *(uint8_t*)(data + 4) = func->data[12];
    *(uint64_t*)&func->data[8] = tmp;

    crypt_function_common(func);
}

void engine(void *a1, void *a2, void *a3, void *a4, void *a5, void *a6) {
    void *ret_addr;
    __asm__ __volatile__ ("mov 16(%%rbp), %%rax" : "=a"(ret_addr) : : "memory");

    generic_func *caller = __builtin_return_address(0) - 5;
    func_node *prev_caller_info = NULL;
    if(prev_caller != (generic_func*)_start) {
        prev_caller_info = func_lookup((long long)prev_caller);
        encrypt_function(prev_caller_info);
    }

    func_node *caller_info = func_lookup((long long)caller);
    decrypt_function(caller_info);

    void *prev_caller_bak = prev_caller;
    prev_caller = caller;

    void *res = caller(a1, a2, a3, a4, a5, a6);

    encrypt_function(caller_info);

    prev_caller = prev_caller_bak;
    if(prev_caller_info != NULL)
        decrypt_function(prev_caller_info);

    __asm__ __volatile__ ("mov %%rax, 8(%%rbp)" : : "a"(ret_addr) : "memory");
    __asm__ __volatile__ ("" : : "a"(res));
} 
