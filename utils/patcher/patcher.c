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
#include <sys/random.h>
#include <sys/wait.h>
#include <sys/uio.h>

#include "aes.h"
#include "funcs_table.h"

#define PAGE_ALIGN 4096

typedef struct {
    const char *filepath;
    uint8_t    *mem;
    size_t     size;
    Elf64_Ehdr *ehdr;
    Elf64_Phdr *phdrs;
    Elf64_Shdr *shdrs;
    Elf64_Sym  *symbols;
    size_t     num_symbols;
    char       *sh_names;
    char       *sym_names;
} elf_t;

static const uint8_t call_opcode[] = {0xe8,0x00,0x00,0x00,0x00};
static const uint8_t zeros[PAGE_ALIGN] = {0};

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

    if(elf->ehdr->e_shnum != 0) {
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
    }

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

static inline uint32_t roundup(uint32_t x, uint32_t align) {
    return (x + align - 1) & (~(align - 1));
}

static bool jump_to_engine(
    elf_t *elf,
    funcs_table *funcs,
    Elf64_Sym *func,
    uint64_t engine_func_addr
) {
    Elf64_Shdr *func_shdr = &elf->shdrs[func->st_shndx];
    uint8_t *func_start = elf->mem + func_shdr->sh_offset +
                          func->st_value - func_shdr->sh_addr;

    if(!funcs_insert(funcs, func->st_value, func_start))
        return false;

    memcpy(func_start, call_opcode, sizeof(call_opcode));
    int32_t jump_diff = engine_func_addr - func->st_value - sizeof(call_opcode);
    memcpy(&func_start[1], &jump_diff, sizeof(jump_diff));

    return true;
}

static bool infect_engine(elf_t *elf, elf_t *engine, const char *patched_executable) {
    bool result = false;
    funcs_table *funcs = NULL;
    int fd = -1;

    Elf64_Shdr *engine_text = section_by_name(engine, ".text");
    if(!engine_text) {
        fputs("engine has not .text section\n", stderr);
        goto exit;
    }

    Elf64_Sym *engine_func = symbol_by_name(engine, "engine");
    if(!engine_func) {
        fputs("engine has not engine function\n", stderr);
        goto exit;
    }

    Elf64_Sym *engine_funcs_table = symbol_by_name(engine, "funcs_table");
    if(!engine_funcs_table) {
        fputs("engine has not funcs_table\n", stderr);
        goto exit;
    }

    size_t num_functions = engine_funcs_table->st_size / sizeof(func_node);
    funcs = funcs_create(num_functions);
    if(!funcs)
        goto exit;

    Elf64_Addr orig_p_vaddr = elf->phdrs[0].p_vaddr;
    size_t rounded_size = roundup(engine_text->sh_size, PAGE_ALIGN);
    uint64_t new_base_addr = orig_p_vaddr - rounded_size;
    uint64_t engine_func_addr = new_base_addr +
                                engine_func->st_value - engine_text->sh_addr;

    for(size_t i = 0; i < elf->num_symbols; i++) {
        if(ELF64_ST_TYPE(elf->symbols[i].st_info) != STT_FUNC)
            continue;

        if(!jump_to_engine(elf, funcs, &elf->symbols[i], engine_func_addr))
            goto exit;
    }

    uint8_t *funcs_table_start = engine->mem + engine_text->sh_offset +
                                 engine_funcs_table->st_value - engine_text->sh_addr;
    memcpy(funcs_table_start, funcs->nodes, engine_funcs_table->st_size);

    // Reverse text infection
    elf->ehdr->e_shoff += rounded_size;
    for(size_t i = 0; i < elf->ehdr->e_shnum; i++) {
        if(elf->shdrs[i].sh_offset >= elf->phdrs[0].p_offset + elf->phdrs[0].p_filesz)
            elf->shdrs[i].sh_offset += rounded_size;
    }
    Elf64_Shdr *text_section = section_by_name(elf, ".text");
    if(text_section) {
        text_section->sh_addr -= rounded_size;
        text_section->sh_size += rounded_size;
    }

    elf->phdrs[0].p_vaddr -= rounded_size;
    elf->phdrs[0].p_paddr -= rounded_size;
    elf->phdrs[0].p_filesz += rounded_size;
    elf->phdrs[0].p_memsz += rounded_size;
    
    for(size_t i = 1; i < elf->ehdr->e_phnum; i++) {
        if(elf->phdrs[i].p_offset > elf->phdrs[0].p_offset)
            elf->phdrs[i].p_offset += rounded_size;
    }

    fd = open(patched_executable, O_CREAT | O_TRUNC | O_WRONLY, 0755);
    if(fd < 0) {
        perror("infect_engine (open)");
        goto exit;
    }

    // write all headers
    size_t to_program_headers = elf->phdrs[0].p_offset;
    if(!write_all(fd, elf->mem, to_program_headers))
        goto exit;
    // write engine
    if(!write_all(fd, engine->mem + engine_text->sh_offset, engine_text->sh_size))
        goto exit;
    // write zeros
    size_t num_zeros = rounded_size - engine_text->sh_size;
    if(num_zeros != 0) {
        if(!write_all(fd, zeros, num_zeros))
            goto exit;
    }
    // write rest of the original elf
    if(!write_all(fd, &elf->mem[to_program_headers], elf->size - to_program_headers))
        goto exit;

    result = true;
exit:
    if(funcs) funcs_free(funcs);
    if(fd >= 0) close(fd);
    return result;
}

static bool write_encrypted_exec(elf_t *elf, const char *patched_executable) {
    int fd = open(patched_executable, O_WRONLY | O_CREAT | O_TRUNC, 0755);
    bool result = true;

    if(fd < 0) {
        perror("write_encrypted_exec (open)");
        return false;
    }

    if(!write_all(fd, elf->mem, elf->size)) {
        result = false;
    }

    close(fd);
    return result;
}

static bool strip_exec(elf_t *elf, const char *stripped_executable) {
    int fd = open(stripped_executable, O_WRONLY | O_CREAT | O_TRUNC, 0755);
    bool result = false;
    if(fd < 0) {
        perror("strip_exec (open)");
        goto exit;
    }

    elf->ehdr->e_phnum = 1;
    size_t orig_p_offset = elf->phdrs[0].p_offset;
    size_t orig_p_size = elf->phdrs[0].p_filesz;
    size_t sizeup = roundup(sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr), PAGE_ALIGN);
    elf->phdrs[0].p_offset = 0;
    elf->phdrs[0].p_vaddr -= sizeup;
    elf->phdrs[0].p_paddr -= sizeup;
    elf->phdrs[0].p_filesz += sizeup;
    elf->phdrs[0].p_memsz += sizeup;

    if(!write_all(fd, elf->mem, sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr)))
        goto exit;

    if(!write_all(fd, zeros, sizeup - sizeof(Elf64_Ehdr) - sizeof(Elf64_Phdr)))
        goto exit;

    if(!write_all(fd, elf->mem + orig_p_offset, orig_p_size))
        goto exit;

    result = true;
exit:
    if(fd >= 0) close(fd);
    return result;
}

typedef struct {
    uint64_t addr;
    uint32_t size;
    uint8_t  iv[16];
    uint8_t  key[240];
} func_data;

static void dump_key_iv(elf_t *elf, Elf64_Sym *func, func_data *data) {
    char buf[512];

    snprintf(buf, sizeof(buf), "../dumps/%s_key", &elf->sym_names[func->st_name]);
    int fd = open(buf, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if(fd < 0) {
        perror("dump_key_iv (open)");
        return;
    }

    if(!write_all(fd, data->key, sizeof(data->key))) {
        close(fd);
        return;
    }
    close(fd);

    snprintf(buf, sizeof(buf), "../dumps/%s_iv", &elf->sym_names[func->st_name]);
    fd = open(buf, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if(fd < 0) {
        perror("dump_key_iv (open)");
        return;
    }

    if(!write_all(fd, data->iv, sizeof(data->iv))) {
        close(fd);
        return;
    }
    close(fd);
    return;
}

static void encrypt_function(elf_t *elf, Elf64_Sym *func, func_data *data) {
    getrandom(data->key, sizeof(data->key), 0);
    getrandom(data->iv, sizeof(data->iv), 0);
    data->addr = func->st_value;
    data->size = roundup(func->st_size, AES_BLOCKLEN);

    dump_key_iv(elf, func, data);

    Elf64_Shdr *func_shdr = &elf->shdrs[func->st_shndx];
    uint8_t *func_start = elf->mem + func_shdr->sh_offset +
                          func->st_value - func_shdr->sh_addr;

    struct AES_ctx ctx = (struct AES_ctx) { .RoundKey = data->key, .Iv = data->iv };
    AES_CBC_encrypt_buffer(&ctx, func_start, data->size);
}

#define UINT64_MAX_STRING_SIZE 20
#define UINT32_MAX_STRING_SIZE 10
#define OPT_SIZE 2

static char** prepare_generate_add_round_key_argv(
    func_data *funcs_data,
    size_t num_functions,
    char **_strings
) {
    char **argv = NULL, *strings = NULL;
    size_t argc = 4 * (num_functions + 1);

    argv = malloc(sizeof(char*) * (argc + 1));
    if(!argv)
        goto error;

    size_t strings_size = num_functions * (UINT64_MAX_STRING_SIZE + 1 +
                                           UINT32_MAX_STRING_SIZE + 1 +
                                           2*(OPT_SIZE + 1));
    strings = malloc(strings_size);
    if(!strings)
        goto error;

    argv[0] = "cabal";
    argv[1] = "exec";
    argv[2] = "gen-add-round-key";
    argv[3] = "--";
    argv[argc] = NULL;

    size_t strings_offset = 0, argv_offset = 4;
    for(size_t i = 0; i < num_functions; i++) {
        argv[argv_offset++] = &strings[strings_offset];
        memcpy(&strings[strings_offset], "-a", 3);
        strings_offset += 3;
        argv[argv_offset++] = &strings[strings_offset];
        strings_offset += sprintf(&strings[strings_offset], "%lu", funcs_data[i].addr);
        strings_offset++;
        argv[argv_offset++] = &strings[strings_offset];
        memcpy(&strings[strings_offset], "-s", 3);
        strings_offset += 3;
        argv[argv_offset++] = &strings[strings_offset];
        strings_offset += sprintf(&strings[strings_offset], "%u", funcs_data[i].size);
        strings_offset++;
    }

    *_strings = strings;
    return argv;
error:
    perror("prepare_generate_add_round_key_argv (malloc)");
    if(argv) free(argv);
    if(strings) free(strings);
    return NULL;
}

static bool generate_add_round_key(
    func_data *funcs_data,
    size_t num_functions,
    const char *addr_round_key
) {
    int pipefd[2];
    if(pipe(pipefd) < 0) {
        perror("generate_add_round_key (pipe)");
        return false;
    }

    pid_t pid = fork();
    if (pid == 0) {
        char **argv = NULL, *strings = NULL;
        argv = prepare_generate_add_round_key_argv(funcs_data, num_functions, &strings);
        if(!argv)
            exit(EXIT_FAILURE);

        if(dup2(pipefd[0], 0) < 0) {
            perror("generate_add_round_key (dup2)");
            exit(EXIT_FAILURE);
        }
        close(pipefd[0]);
        close(pipefd[1]);

        int fd = open(addr_round_key, O_CREAT | O_WRONLY | O_TRUNC, 0644);
        if(fd < 0) {
            perror("generate_add_round_key (open)");
            exit(EXIT_FAILURE);
        }
        if(dup2(fd, 1) < 0) {
            perror("generate_add_round_key (dup2)");
            exit(EXIT_FAILURE);
        }

        if(execvp("cabal", argv) < 0) {
            perror("generate_add_round_key (execvp)");
            exit(EXIT_FAILURE);
        }
    } else if(pid < 0) {
        perror("generate_add_round_key (fork)");
        return false;
    }

    close(pipefd[0]);

    for(size_t i = 0; i < num_functions; i++) {
        if(!write_all(pipefd[1], funcs_data[i].iv, sizeof(funcs_data[i].iv)))
            return false;
        if(!write_all(pipefd[1], funcs_data[i].key, sizeof(funcs_data[i].key)))
            return false;
    }
    close(pipefd[1]);

    int wstatus;
    if(waitpid(pid, &wstatus, 0) < 0) {
        perror("generate_add_round_key (waitpid)");
        return false;
    }
    if(WEXITSTATUS(wstatus) != 0) {
        return false;
    }
    return true;
}

static bool encrypt_and_generate_add_round_key(
    elf_t *elf,
    size_t num_functions,
    const char* addr_round_key
) {
    bool result = false;
    func_data *funcs_data = malloc(sizeof(func_data) * num_functions);
    if(!funcs_data) {
        perror("encrypt_and_generate_add_round_key (malloc)");
        goto exit;
    }

    size_t func_idx = 0;
    for(size_t i = 0; i < elf->num_symbols; i++) {
        if(ELF64_ST_TYPE(elf->symbols[i].st_info) != STT_FUNC)
            continue;
        encrypt_function(elf, &elf->symbols[i], &funcs_data[func_idx++]);
    }

    if(!generate_add_round_key(funcs_data, num_functions, addr_round_key))
        goto exit;

    result = true;
exit:
    if(funcs_data) free(funcs_data);
    return result;
}

typedef enum {
    ENCRYPT,
    STRIP,
    INFECT,
    HELP
} action_t;

int main(int argc, char **argv) {
    int result = EXIT_FAILURE;
    elf_t elf, engine;
    action_t action = HELP;
    char *executable, *table, *addr_round_key, *patched_executable, *engine_path;

    engine.mem = MAP_FAILED; 

    if(argc == 6 && !strcmp(argv[1], "-e")) {
        executable = argv[2];
        table = argv[3];
        addr_round_key = argv[4];
        patched_executable = argv[5];
        action = ENCRYPT;
    } else if(argc == 5 && !strcmp(argv[1], "-p")) {
        executable = argv[2];
        patched_executable = argv[3];
        engine_path = argv[4];
        action = INFECT;
    } else if(argc == 4 && !strcmp(argv[1], "-s")) {
        executable = argv[2];
        patched_executable = argv[3];
        action = STRIP;
    }

    if(action == HELP) {
        fprintf(
            stderr,
            "Usage:\n\t%s -e <executable> <table.inc> <add_round_key.c> "
            "<patched_executable>\n"
            "\t%s -p <executable> <patched_executable> <engine>\n"
            "\t%s -s <executable> <stripped_executable>\n",
            argv[0],
            argv[0],
            argv[0]
        );
        return EXIT_FAILURE;
    }

    srand48(time(NULL));

    if(!open_elf(executable, &elf))
        goto exit;

    if(action == ENCRYPT) {
        size_t num_functions = count_functions(&elf);
        if(!encrypt_and_generate_add_round_key(&elf, num_functions, addr_round_key))
            goto exit;
        num_functions = get_func_table_size(num_functions);
        if(!write_table_inc(table, num_functions))
            goto exit;
        if(!write_encrypted_exec(&elf, patched_executable))
            goto exit;
    } else if(action == INFECT) {
        if(!open_elf(engine_path, &engine))
            goto exit;
        if(!infect_engine(&elf, &engine, patched_executable))
            goto exit;
    } else {
        if(!strip_exec(&elf, patched_executable))
            goto exit;
    }

    result = EXIT_SUCCESS;
exit:
    if(elf.mem != MAP_FAILED) close_elf(&elf);
    if(engine.mem != MAP_FAILED) close_elf(&engine);
    return result;
}
