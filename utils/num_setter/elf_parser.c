#include "elf_parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <elf.h>
#include "offset_table.h"
#include "file_map.h"

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

static inline patch_node* patch_insert(patch_node *patch, size_t offset, int index) {
    patch_node *new_node = malloc(sizeof(patch_node));
    if(!new_node) {
        perror("patch_insert");
        return NULL;
    }

    new_node->offset = offset;
    new_node->nanocall_index = index;
    new_node->next = patch;
    return new_node;
}

static inline patch_node* parse_nanocalls(file_map *file, htable_t *offsets) {
    Elf64_Ehdr *ehdr = file->data;
    Elf64_Shdr *shdr = file->data + ehdr->e_shoff;
    offset_node off_key;

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
    char *strtab = file->data + strtab_shdr->sh_offset;

    patch_node *patch_list = NULL;
    Elf64_Sym *symtab = file->data + symtab_shdr->sh_offset;
    size_t num_syms = symtab_shdr->sh_size / sizeof(Elf64_Sym);
    for(size_t i = 0; i < num_syms; i++) {
        Elf64_Sym *sym = &symtab[i];
        char *sym_name = &strtab[sym->st_name];
        if(strncmp(sym_name, "nano_", 5) != 0)
            continue;

        Elf64_Shdr *sym_shdr = &shdr[sym->st_shndx];
        size_t offset = sym_shdr->sh_offset + sym->st_value - sym_shdr->sh_addr;

        char *under = strrchr(sym_name, '_');
        *under = 0;
        off_key.call_name = sym_name;
        off_key.call_name_length = under - sym_name;
        offset_node *call_offset = (offset_node*)htable_lookup(offsets, (hnode_t*)&off_key);
        if(!call_offset) {
            fprintf(stderr, "Failed to get index for %s nanocall\n", sym_name);
            patch_free(patch_list);
            return NULL;
        }
        *under = '_';

        long addr = sym->st_value;
        uint32_t hash = default_hash_func((uint8_t*)&addr, sizeof(addr));
        int index = call_offset->offset - hash % offsets->num_nodes;
        patch_node *new_list = patch_insert(patch_list, offset, index);
        if(!new_list) {
            patch_free(patch_list);
            return NULL;
        }
        patch_list = new_list;
    }

    return patch_list;
}

patch_node* parse_elf(file_map *file, htable_t *offsets) {
    if(!check_elf(file))
        return NULL;

    return parse_nanocalls(file, offsets);
}

void patch_free(patch_node *patch_list) {
    while(patch_list) {
        patch_node *next = patch_list->next;
        free(patch_list);
        patch_list = next;
    }
}
