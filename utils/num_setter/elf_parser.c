#include "elf_parser.h"

#include <stdio.h>
#include <string.h>
#include <elf.h>
#include "addr_table.h"
#include "file_map.h"

typedef struct {
    char       *name;
    Elf64_Addr start_addr, end_addr;
} func_t;

static inline bool check_elf(FileMap *file) {
    void *base_ptr = FileMapGetPointer(file);
    Elf64_Ehdr *ehdr = base_ptr;

    if(memcmp(ehdr->e_ident, ELFMAG, SELFMAG) != 0) {
        fputs("Invalid ELF signature\n", stderr);
        return false;
    }

    if(ehdr->e_ident[EI_VERSION] != EV_CURRENT || ehdr->e_version != EV_CURRENT) {
        fputs("Invald ELF version\n", stderr);
        return false;
    }

    if(ehdr->e_ident[EI_CLASS] != ELFCLASS64) {
        fputs("Only 64-bit ELF are supported\n", stderr);
        return false;
    }

    return true;
}

static inline Elf64_Shdr* section_by_name(FileMap *file, const char *name) {
    void *base_ptr = FileMapGetPointer(file);
    Elf64_Ehdr *ehdr = base_ptr;
    Elf64_Shdr *shdr = base_ptr + ehdr->e_shoff;
    char *strtab = base_ptr + shdr[ehdr->e_shstrndx].sh_offset;

    for(size_t i = 0; i < ehdr->e_shnum; i++) {
        if(!strcmp(&strtab[shdr[i].sh_name], name))
            return &shdr[i];
    }

    return NULL;
}

static inline Elf64_Shdr* section_by_type(FileMap *file, Elf64_Word type) {
    void *base_ptr = FileMapGetPointer(file);
    Elf64_Ehdr *ehdr = base_ptr;
    Elf64_Shdr *shdr = base_ptr + ehdr->e_shoff;

    for(size_t i = 0; i < ehdr->e_shnum; i++) {
        if(shdr[i].sh_type == type)
            return &shdr[i];
    }

    return NULL;
}

static inline vector_t* parse_funcs(FileMap *file) {
    void *base_ptr = FileMapGetPointer(file);
    Elf64_Shdr *symtab_shdr = section_by_type(file, SHT_SYMTAB);
    if(!symtab_shdr) {
        fputs("ELF must have a .symtab section\n", stderr);
        return NULL;
    }

    Elf64_Shdr *strtab_shdr = section_by_name(file, ".strtab");
    if(!strtab_shdr) {
        fputs("ELF must have a .strtab section\n", stderr);
        return NULL;
    }
    char *strtab = base_ptr + strtab_shdr->sh_offset;

    vector_t *funcs = vector_new(8, sizeof(func_t));
    if(!funcs)
        return NULL;

    Elf64_Sym *symtab = base_ptr + symtab_shdr->sh_offset;
    size_t num_syms = symtab_shdr->sh_size / sizeof(Elf64_Sym);
    for(size_t i = 0; i < num_syms; i++) {
        if(ELF64_ST_TYPE(symtab[i].st_info) != STT_FUNC)
            continue;

        func_t func = {
            .name = &strtab[symtab[i].st_name],
            .start_addr = symtab[i].st_value,
            .end_addr = symtab[i].st_value + symtab[i].st_size,
        };

        if(!vector_append(funcs, &func)) {
            vector_free(funcs);
            return NULL;
        }
    }

    return funcs;
}

static inline func_t* func_by_symbol(vector_t *funcs, Elf64_Sym *sym) {
    for(size_t i = 0; i < funcs->length; i++) {
        func_t *func = funcs->data + i * funcs->elem_size;
        if(sym->st_value >= func->start_addr &&
           sym->st_value + sym->st_size <= func->end_addr)
            return func;
    }
    return NULL;
}

static inline htable_t* parse_nanocalls(FileMap *file, vector_t *funcs) {
    void *base_ptr = FileMapGetPointer(file);
    Elf64_Shdr *symtab_shdr = section_by_type(file, SHT_SYMTAB);
    if(!symtab_shdr) {
        fputs("ELF must have a .symtab section\n", stderr);
        return NULL;
    }

    Elf64_Shdr *strtab_shdr = section_by_name(file, ".strtab");
    if(!strtab_shdr) {
        fputs("ELF must have a .strtab section\n", stderr);
        return NULL;
    }
    char *strtab = base_ptr + strtab_shdr->sh_offset;

    htable_t *addr_table = addr_table_create(8);
    if(!addr_table)
        return NULL;

    Elf64_Sym *symtab = base_ptr + symtab_shdr->sh_offset;
    size_t num_syms = symtab_shdr->sh_size / sizeof(Elf64_Sym);
    for(size_t i = 0; i < num_syms; i++) {
        char *sym_name = &strtab[symtab[i].st_name];
        if(strncmp(sym_name, "nano_syscall_", 13) != 0)
            continue;

        func_t *func = func_by_symbol(funcs, &symtab[i]);
        if(!func) {
            fprintf(stderr, "Failed to find function for symbol %s\n", sym_name);
            htable_free(addr_table);
            return NULL;
        }

        if(!addr_node_insert(addr_table, func->name, symtab[i].st_value)) {
            htable_free(addr_table);
            return NULL;
        }
    }

    return addr_table;
}

htable_t* parse_elf(const char *filename) {
    htable_t *addr_table = NULL;
    vector_t *funcs = NULL;
    FileMap *file = NULL;

    file = FileMapCreate(filename);
    if(!file)
        goto exit;

    if(!check_elf(file))
        goto exit;

    funcs = parse_funcs(file);
    if(!funcs)
        goto exit;

    addr_table = parse_nanocalls(file, funcs);
exit:
    if(file) FileMapDestroy(file);
    if(funcs) vector_free(funcs);
    return addr_table;
}
