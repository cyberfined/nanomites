#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <elf.h>
#include "file_map.h"
#include "offset_parser.h"
#include "nanocalls_table.h"

#define CMD_SYMBOL       "cmd_table"
#define NANOCALLS_SYMBOL "nanocalls_table"

static inline bool check_elf(file_map *file) {
    Elf64_Ehdr *ehdr = file->data;

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

static inline Elf64_Shdr* section_by_name(file_map *file, const char *name) {
    Elf64_Ehdr *ehdr = file->data;
    Elf64_Shdr *shdr = file->data + ehdr->e_shoff;
    char *strtab = file->data + shdr[ehdr->e_shstrndx].sh_offset;

    for(size_t i = 0; i < ehdr->e_shnum; i++) {
        if(!strcmp(&strtab[shdr[i].sh_name], name))
            return &shdr[i];
    }

    return NULL;
}

static inline Elf64_Shdr* section_by_type(file_map *file, Elf64_Word type) {
    Elf64_Ehdr *ehdr = file->data;
    Elf64_Shdr *shdr = file->data + ehdr->e_shoff;

    for(size_t i = 0; i < ehdr->e_shnum; i++) {
        if(shdr[i].sh_type == type)
            return &shdr[i];
    }

    return NULL;
}

static inline Elf64_Sym* symbol_by_name(file_map *file, const char *name) {
    Elf64_Shdr *symtab_shdr = section_by_type(file, SHT_SYMTAB);
    if(!symtab_shdr) {
        fputs("ELF must have a .symtab section\n", stderr);
        return NULL;
    }

    Elf64_Shdr *strtab_shdr = section_by_name(file, ".strtab");
    if(!strtab_shdr)
        return NULL;

    char *strtab = file->data + strtab_shdr->sh_offset;
    Elf64_Sym *symtab = file->data + symtab_shdr->sh_offset;
    size_t num_syms = symtab_shdr->sh_size / sizeof(Elf64_Sym);

    for(size_t i = 0; i < num_syms; i++) {
        Elf64_Sym *sym = &symtab[i];
        char *sym_name = &strtab[sym->st_name];
        if(!strcmp(sym_name, name))
            return sym;
    }

    return NULL;
}

static inline bool patch_nanocalls(file_map *file, htable_t *offsets) {
    nanocalls_table *nanocalls = NULL;
    offset_node off_key;
    bool res = false;

    Elf64_Shdr *symtab_shdr = section_by_type(file, SHT_SYMTAB);
    if(!symtab_shdr) {
        fputs("ELF must have a .symtab section\n", stderr);
        goto error;
    }

    Elf64_Shdr *strtab_shdr = section_by_name(file, ".strtab");
    if(!strtab_shdr) {
        fputs("ELF must have a .strtab section\n", stderr);
        goto error;
    }
    char *strtab = file->data + strtab_shdr->sh_offset;

    Elf64_Sym *cmd_sym = symbol_by_name(file, CMD_SYMBOL);
    if(!cmd_sym) {
        fputs("ELF must have " CMD_SYMBOL " symbol\n", stderr);
        goto error;
    }
    size_t cmd_size = cmd_sym->st_size / offsets->num_nodes;

    Elf64_Sym *nanocalls_sym = symbol_by_name(file, NANOCALLS_SYMBOL);
    if(!nanocalls_sym) {
        fputs("ELF must have " NANOCALLS_SYMBOL " symbol\n", stderr);
        goto error;
    }
    size_t num_nanocalls = nanocalls_sym->st_size / sizeof(nanocall_node);

    nanocalls = nanocalls_create(num_nanocalls);
    if(!nanocalls)
        goto error;
    if(nanocalls->max_nodes > num_nanocalls) {
        fputs("ELF has wrong " NANOCALLS_SYMBOL " symbol size\n", stderr);
        goto error;
    }

    Elf64_Sym *symtab = file->data + symtab_shdr->sh_offset;
    size_t num_syms = symtab_shdr->sh_size / sizeof(Elf64_Sym);
    for(size_t i = 0; i < num_syms; i++) {
        Elf64_Sym *sym = &symtab[i];
        char *sym_name = &strtab[sym->st_name];
        if(strncmp(sym_name, "nano_", 5) != 0)
            continue;

        char *under = strrchr(sym_name, '_');
        *under = 0;
        off_key.call_name = sym_name;
        off_key.call_name_length = under - sym_name;
        offset_node *call_offset = (offset_node*)htable_lookup(offsets, (hnode_t*)&off_key);
        if(!call_offset) {
            fprintf(stderr, "Failed to get index for %s nanocall\n", sym_name);
            goto error;
        }
        *under = '_';

        void *nanocall = (void*)(cmd_sym->st_value + call_offset->offset * cmd_size);
        if(!nanocalls_insert(nanocalls, sym->st_value, nanocall))
            goto error;
    }

    Elf64_Ehdr *ehdr = file->data;
    Elf64_Shdr *shdr = file->data + ehdr->e_shoff;
    Elf64_Shdr *nanocalls_shdr = &shdr[nanocalls_sym->st_shndx];
    size_t nanocalls_offset = nanocalls_shdr->sh_offset + nanocalls_sym->st_value -
        nanocalls_shdr->sh_addr;
    memcpy(file->data + nanocalls_offset, nanocalls->nodes, nanocalls_sym->st_size);

    res = true;
error:
    if(nanocalls) nanocalls_free(nanocalls);
    return res;
}

int main(int argc, char **argv) {
    if(argc != 4) {
        fprintf(stderr,
                "Usage: %s <executable> <table.h> <out>\n"
                "Util for set magical numbers for nanosyscalls\n",
                argv[0]);
        return 1;
    }

    const char *executable = argv[1];
    const char *offsets_file = argv[2];
    const char *output = argv[3];

    htable_t *offsets = NULL;
    file_map *elf = NULL;
    int res = 1;

    elf = file_map_open(executable);
    if(!elf)
        goto cleanup;

    offsets = parse_offsets(offsets_file);
    if(!offsets)
        goto cleanup;

    if(!patch_nanocalls(elf, offsets))
        goto cleanup;

    if(!file_map_write(elf, output))
        goto cleanup;

    res = 0;
cleanup:
    if(elf) file_map_destroy(elf);
    if(offsets) htable_free(offsets);
    return res;
}
