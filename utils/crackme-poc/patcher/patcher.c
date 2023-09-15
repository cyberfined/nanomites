#define _DEFAULT_SOURCE 1

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <elf.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include "funcs_table.h"

typedef struct {
    const char *filepath;
    uint8_t    *mem;
    size_t     size;
    Elf64_Ehdr *ehdr;
    Elf64_Phdr *phdrs;
    Elf64_Shdr *shdrs;
    Elf64_Sym  *symbols;
    uintptr_t  engine_addr;
    size_t     num_symbols;
    char       *sh_names;
    char       *sym_names;
} elf_t;

static const char* decrypt_functions[] = {
    "jenkins_hash_func",
    "func_lookup",
    "engine",
    "crypt_function_common",
    "encrypt_function",
    "decrypt_function",
    "_start"
};
#define NUM_DECRYPT_FUNCS sizeof(decrypt_functions)/sizeof(*decrypt_functions)

static const uint8_t call_opcode[] = {0xe8,0x00,0x00,0x00,0x00};

static Elf64_Shdr* section_by_type(elf_t *elf, uint32_t sh_type) {
    for(size_t i = 0; i < elf->ehdr->e_shnum; i++) {
        if(elf->shdrs[i].sh_type == sh_type)
            return &elf->shdrs[i];
    }
    return NULL;
}

static Elf64_Shdr* section_by_name(elf_t *elf, const char *name) {
    for(size_t i = 0; i < elf->ehdr->e_shnum; i++) {
        char *sh_name = &elf->sh_names[elf->shdrs[i].sh_name];
        if(!strcmp(sh_name, name))
            return &elf->shdrs[i];
    }
    return NULL;
}

static Elf64_Sym* symbol_by_name(elf_t *elf, const char *name) {
    for(size_t i = 0; i < elf->num_symbols; i++) {
        char *sym_name = &elf->sym_names[elf->symbols[i].st_name];
        if(!strcmp(sym_name, name))
            return &elf->symbols[i];
    }
    return NULL;
}

static bool open_elf(const char *filepath, elf_t *elf) {
    elf->mem = MAP_FAILED;

    int fd = open(filepath, O_RDONLY);
    if(fd < 0) {
        perror("open_elf (open)");
        goto error;
    }

    struct stat statbuf;
    if(fstat(fd, &statbuf) < 0) {
        perror("open_elf (fstat)");
        goto error;
    }

    elf->filepath = filepath;
    elf->size = statbuf.st_size;
    elf->mem = mmap(NULL, elf->size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
    if(elf->mem == MAP_FAILED) {
        perror("open_elf (mmap)");
        goto error;
    }

    elf->ehdr = (Elf64_Ehdr*)elf->mem;
    elf->phdrs = (Elf64_Phdr*)(elf->mem + elf->ehdr->e_phoff);
    elf->shdrs = (Elf64_Shdr*)(elf->mem + elf->ehdr->e_shoff);
    elf->sh_names = (char*)elf->mem + elf->shdrs[elf->ehdr->e_shstrndx].sh_offset;

    Elf64_Shdr *sh_strtab = section_by_name(elf, ".strtab");
    if(!sh_strtab) {
        fputs("Failed to find strtab section\n", stderr);
        goto error;
    }
    elf->sym_names = (char*)elf->mem + sh_strtab->sh_offset;

    Elf64_Shdr *sym_shdr = section_by_type(elf, SHT_SYMTAB);
    if(!sym_shdr) {
        fputs("Executable does not have a symtab\n", stderr);
        goto error;
    }
    elf->symbols = (void*)elf->mem + sym_shdr->sh_offset;
    elf->num_symbols = sym_shdr->sh_size / sizeof(Elf64_Sym);

    Elf64_Sym *engine_func = symbol_by_name(elf, "engine");
    if(!engine_func) {
        fputs("Executable does not have an engine function\n", stderr);
        goto error;
    }
    elf->engine_addr = engine_func->st_value;

    close(fd);
    return true;
error:
    if(fd >= 0) close(fd);
    if(elf->mem != MAP_FAILED) munmap(elf->mem, elf->size);
    return false;
}

static void close_elf(elf_t *elf) {
    munmap(elf->mem, elf->size);
}

static bool write_all(int fd, const void *buf, size_t size) {
    while(size != 0) {
        ssize_t wb = write(fd, buf, size);
        if(wb < 0)
            return false;
        size -= wb;
    }
    return true;
}

static size_t count_functions(elf_t *elf) {
    size_t num_functions = 0;
    for(size_t i = 0; i < elf->num_symbols; i++) {
        if(ELF64_ST_TYPE(elf->symbols[i].st_info) != STT_FUNC)
            continue;

        char *sym_name = &elf->sym_names[elf->symbols[i].st_name];
        bool should_count = true;
        for(size_t j = 0; j < NUM_DECRYPT_FUNCS; j++) {
            if(!strcmp(decrypt_functions[j], sym_name)) {
                should_count = false;
                break;
            }
        }

        if(should_count)
            num_functions++;
    }

    return num_functions;
}

static bool write_table_inc(const char *table, size_t hash_table_size) {
    FILE *f = fopen(table, "w");
    if(!f) {
        perror("write_table_inc (fopen)");
        return false;
    }
    fprintf(f, "#define FUNCS_TABLE_SIZE %lu\n", hash_table_size);
    fclose(f);
    return true;
}

static bool encrypt_function(elf_t *elf, funcs_table *funcs, Elf64_Sym *func) {
    if(func->st_size < sizeof(call_opcode)) {
        char *func_name = &elf->sym_names[func->st_name];
        fprintf(stderr, "Function: %s is too small\n", func_name);
        return false;
    }

    uint64_t key = lrand48() << 31 | lrand48();
    uint8_t data[16];
    memcpy(data, &key, sizeof(key));

    Elf64_Shdr *func_shdr = &elf->shdrs[func->st_shndx];
    uint8_t *func_start = elf->mem + func_shdr->sh_offset +
                          func->st_value - func_shdr->sh_addr;

    size_t i, j;
    for(i = 0, j = func->st_size / sizeof(uint64_t);
        j != 0;
        i += sizeof(uint64_t), j--)
    {
        uint64_t part;
        memcpy(&part, &func_start[i], sizeof(uint64_t));
        part ^= key;
        memcpy(&func_start[i], &part, sizeof(uint64_t));
    }

    for(j = 0; i < func->st_size; i++, j++)
        func_start[i] ^= (key >> (j * 8)) & 0xff;

    memcpy(&data[8], func_start, sizeof(call_opcode));
    memcpy(func_start, call_opcode, sizeof(call_opcode));
    int32_t jump_diff = elf->engine_addr - func->st_value - sizeof(call_opcode);
    memcpy(&func_start[1], &jump_diff, sizeof(jump_diff));

    return funcs_insert(funcs, func->st_value, func->st_size, data);
}

static bool encrypt_functions(elf_t *elf) {
    Elf64_Sym *funcs_table_sym = symbol_by_name(elf, "funcs_table");
    if(!funcs_table_sym) {
        fputs("Executable does not have funcs_table symbol\n", stderr); 
        return false;
    }
    Elf64_Shdr *funcs_table_shdr = &elf->shdrs[funcs_table_sym->st_shndx];
    uint8_t *funcs_table_start = elf->mem + funcs_table_shdr->sh_offset +
                                 funcs_table_sym->st_value - funcs_table_shdr->sh_addr;

    size_t num_functions = funcs_table_sym->st_size / sizeof(func_node);
    funcs_table *funcs = funcs_create(num_functions);
    if(!funcs)
        return false;

    if(funcs->max_nodes * sizeof(func_node) != funcs_table_sym->st_size) {
        fputs("funcs_table has a wrong size\n", stderr);
        return false;
    }

    for(size_t i = 0; i < elf->num_symbols; i++) {
        if(ELF64_ST_TYPE(elf->symbols[i].st_info) != STT_FUNC)
            continue;

        char *sym_name = &elf->sym_names[elf->symbols[i].st_name];
        bool should_encrypt = true;
        for(size_t j = 0; j < NUM_DECRYPT_FUNCS; j++) {
            if(!strcmp(decrypt_functions[j], sym_name)) {
                should_encrypt = false;
                break;
            }
        }
        if(!should_encrypt)
            continue;

        if(!encrypt_function(elf, funcs, &elf->symbols[i])) {
            funcs_free(funcs);
            return false;
        }
    }

    memcpy(funcs_table_start, funcs->nodes, funcs_table_sym->st_size);

    return true;
}

static bool patch_exec(elf_t *elf, const char *patched_filename) {
    if(!encrypt_functions(elf))
        return false;

    elf->ehdr->e_shnum = 0;
    elf->ehdr->e_shoff = 0;
    elf->ehdr->e_shstrndx = SHN_UNDEF;

    size_t phnum = 0;
    size_t size = 0;
    for(phnum = 0; phnum < elf->ehdr->e_phnum; phnum++) {
        if(elf->phdrs[phnum].p_type != PT_LOAD)
            break;

        size_t next_size = elf->phdrs[phnum].p_offset + elf->phdrs[phnum].p_filesz;
        if(next_size > size)
            size = next_size;
    }
    elf->ehdr->e_phnum = phnum;

    int fd = open(patched_filename, O_WRONLY | O_CREAT | O_TRUNC, 0755);
    if(fd < 0) {
        perror("patch_exec (open)");
        return false;
    }

    if(!write_all(fd, elf->mem, size)) {
        close(fd);
        return false;
    }

    close(fd);
    return true;
}

int main(int argc, char **argv) {
    int result = EXIT_FAILURE;
    elf_t elf;
    bool print_help = false, should_count;
    char *executable, *table, *patched_executable;

    if(argc == 4) {
        if(!strcmp(argv[1], "-c")) {
            should_count = true;
            executable = argv[2];
            table = argv[3];
        } else if(!strcmp(argv[1], "-p")) {
            should_count = false;
            executable = argv[2];
            patched_executable = argv[3];
        } else {
            print_help = true;
        }
    } else {
        print_help = true;
    }

    if(print_help) {
        fprintf(
            stderr,
            "Usage:\n\t%s -c <executable> <table.inc>\n"
            "\t%s -p <executable> <patched_executable>\n",
            argv[0],
            argv[0]
        );
        return EXIT_FAILURE;
    }

    srand48(time(NULL));

    if(!open_elf(executable, &elf))
        goto exit;

    if(should_count) {
        size_t num_functions = count_functions(&elf);
        size_t hash_table_size = get_func_table_size(num_functions);
        if(!hash_table_size) {
            fputs("Function number is too big\n", stderr);
            goto exit;
        }
        if(!write_table_inc(table, hash_table_size))
            goto exit;
    } else if(!patch_exec(&elf, patched_executable)) {
        goto exit;
    }

    result = EXIT_SUCCESS;
exit:
    if(elf.mem != MAP_FAILED) close_elf(&elf);
    return result;
}
