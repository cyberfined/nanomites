#!/usr/bin/env python3
from enum import Enum
import random
import sys

class Register(Enum):
    RDI = 14 * 8
    RSI = 13 * 8
    RDX = 12 * 8
    R10 = 7 * 8
    R8  = 9 * 8
    R9  = 8 * 8

    def inline(self):
        inlines = {Register.RDI: "D",
                   Register.RSI: "S",
                   Register.RDX: "d",
                   Register.R10: "r",
                   Register.R8:  "r",
                   Register.R9:  "r"}
        return inlines[self]

    def __str__(self):
        shows = {Register.RDI: "rdi",
                 Register.RSI: "rsi",
                 Register.RDX: "rdx",
                 Register.R10: "r10",
                 Register.R8:  "r8",
                 Register.R9:  "r9"}
        return shows[self]

    def order(self):
        return list(Register).index(self)

class ArgType(Enum):
    INT  = 0
    ADDR = 1
    VOID = 2

    def strType(self):
        if self is ArgType.INT:
            return "long"
        elif self is ArgType.ADDR:
            return "void*"
        else:
            return "void"

class CmdType(Enum):
    SYSCALL = 0
    PROC    = 1
    REGS    = 2

    def strType(self):
        if self is CmdType.SYSCALL:
            return "CMD_SYSCALL"
        elif self is CmdType.PROC:
            return "CMD_PROC"
        else:
            return "CMD_REGS"

def Syscall(title, argsTypes, retType):
    return Command(CmdType.SYSCALL, title, argsTypes, retType)

def Proc(title, argsTypes, retType):
    return Command(CmdType.PROC, title, argsTypes, retType)

def Regs(title, argsTypes=[]):
    return Command(CmdType.REGS, title, argsTypes, ArgType.VOID)

class Command:
    def __init__(self, cmdType, title, argsTypes, retType):
        self.title = title
        self.cmdType = cmdType
        self.numArgs = len(argsTypes)

        if cmdType is not CmdType.REGS:
            self.args = list(Register)
            random.shuffle(self.args)
            self.args = self.args[:self.numArgs]
            self.argsTypes = argsTypes
        else:
            self.args, self.argsTypes = list(zip(*argsTypes))

        self.retType = retType
        self.number = None

    def inlineFunc(self):
        argsList = ', '.join([f"(long)a{a.order()}" for a in self.args])

        argsDef = [f"{t.strType()} a{i}" for i, t in enumerate(self.argsTypes)]
        argsDef = ", ".join(argsDef)

        yield f"static inline {self.retType.strType()} nano_{self.title}({argsDef}) {{"
        if self.retType is not ArgType.VOID:
            yield  "    long ret;"

        specialRegs = [Register.R10, Register.R8, Register.R9]
        for r in specialRegs:
            idx = next((i for i, t in enumerate(self.args) if r == t), None)
            if idx == None:
                continue
            yield f"    register long {r} __asm__(\"{r}\") = (long)a{idx};"

        colon = " :" if self.retType is not ArgType.VOID else " ::"
        yield f"    __asm__ __volatile__ (\"int3\\n\\t\""
        yield f"                          \"nano_{self.title}_%=:\\n\\t\"{colon}"
        if self.retType is not ArgType.VOID:
            yield  "                          \"=a\"(ret) :"

        argList = []

        for idx, arg in enumerate(self.args):
            if arg in specialRegs:
                argList.append(f"\"{arg.inline()}\"({arg})")
            else:
                argList.append(f"\"{arg.inline()}\"(a{idx})")
        argList = ' '*26 + ', '.join(argList) + " :"
        yield argList
        yield  "                          \"rcx\", \"r11\", \"memory\");"

        if self.retType is not ArgType.VOID:
            yield f"    return ({self.retType.strType()})ret;"
        yield "}"

    def tableEntries(self):
        argsStr = [hex(off.value) for off in self.args]
        argsStr += ['0x00'] * (len(list(Register)) - self.numArgs)
        argsStr = "{" + ','.join(argsStr) + "}"

        yield f"{{.type = {self.cmdType.strType()},"
        if self.cmdType is not CmdType.REGS:
            if self.cmdType is CmdType.SYSCALL:
                yield f" .number = {self.number},"
            elif self.cmdType is CmdType.PROC:
                yield f" .proc_func = {self.title},"
            yield f" .num_args = {self.numArgs},"
            yield f" .args_offsets = {argsStr}"
        else:
            yield f" .regs_proc = {self.title}"
        yield "},"

class TableGen:
    def __init__(self, includePath, commands):
        self.includePath = includePath
        self.commands = commands
        random.shuffle(self.commands)
        self.entriesOffsets = []
        self.parseSycallNums()

    def parseSycallNums(self):
        with open(self.includePath) as inp:
            callNums = dict()
            for line in inp:
                line = line.strip()
                if not line.startswith("#define"):
                    continue
                toks = list(filter(lambda x: len(x) > 0, line.split(" ")))
                if len(toks) != 3 or not toks[1].startswith("__NR_"):
                    continue
                callNums[toks[1][5:]] = int(toks[2])

        for cmd in self.commands:
            if cmd.cmdType is not CmdType.SYSCALL:
                continue
            if cmd.title not in callNums:
                raise RuntimeError(f"Unknown syscall {cmd.title}")
            cmd.number = callNums[cmd.title]

    def generateHeaderFile(self):
        header = """\
#pragma once

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <sys/user.h>

#define CMD_SYSCALL 1337
#define CMD_PROC    1338
#define CMD_REGS    1339

typedef void (*cmd_regs_proc)(struct user_regs_struct*);

typedef struct {
    uint32_t type;
    union {
        struct {
            uint16_t num_args;
            uint16_t args_offsets[6];
            union {
                uint16_t number;
                void     *proc_func;
            };
        };
        cmd_regs_proc regs_proc;
    };
} cmd_entry;

typedef struct {
    long long addr;
    void      *nanocall;
} nanocall_node;

extern cmd_entry cmd_table[];
extern uint16_t fun_args_offsets[];
extern nanocall_node nanocalls_table[31];

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

static inline void* nano_lookup(long long addr) {
    uint32_t hash = jenkins_hash_func(addr);
    int cur = hash % (sizeof(nanocalls_table)/sizeof(*nanocalls_table));
    int step = hash % (sizeof(nanocalls_table)/sizeof(*nanocalls_table) - 2) + 1;
    int nstep = step;

    for(size_t i = 0; i < sizeof(nanocalls_table)/sizeof(*nanocalls_table); i++) {
        nanocall_node *cur_node = &nanocalls_table[cur];
        if(!cur_node->addr)
            break;
        if(cur_node->addr == addr)
            return cur_node->nanocall;
        cur = (hash + nstep) % (sizeof(nanocalls_table)/sizeof(*nanocalls_table));
        nstep += step;
    }

    return NULL;
}
"""
        return '\n'.join([header, self.generateMacro(), self.generateFuncs()])

    def generateFuncs(self):
        lines = []
        for idx, cmd in enumerate(self.commands):
            for line in cmd.inlineFunc():
                lines.append(line)
            if idx != len(self.commands)-1:
                lines.append("")
        lines.append("")
        return '\n'.join(lines)

    def generateMacro(self):
        lines = []
        maxTitleLen = max(len(cmd.title) for cmd in self.commands)
        for idx, cmd in enumerate(self.commands):
            spaces = ' ' * (maxTitleLen - len(cmd.title) + 1)
            lines.append(f"#define NANO_{cmd.title.upper()}_OFFSET{spaces}{idx}")
        spaces = ' ' * (maxTitleLen + 2)
        lines.append(f"#define NUM_ENTRIES{spaces}{len(self.commands)}")
        lines.append("")
        return '\n'.join(lines)

    def generateTable(self):
        offsets = ", ".join([hex(r.value) for r in Register])
        table = [f"uint16_t fun_args_offsets[] = {{{offsets}}};"]
        table.append("")
        table.append("cmd_entry cmd_table[] = {")
        for cmd in self.commands:
            for entry in cmd.tableEntries():
                table.append("    " + entry)
        table.append("};\n")
        table.append("nanocall_node nanocalls_table[31] = {")
        table.append("    {.addr = 0x1, .nanocall = (void*)0x1}")
        table.append("};\n")
        return '\n'.join(table)

if len(sys.argv) != 2:
    print(f"Usage: {sys.argv[0]} <filename>")
    sys.exit(0)

with open(sys.argv[1]) as cmdFile:
    commands = eval(cmdFile.read())

tableGen = TableGen("/usr/include/asm/unistd_64.h", commands)

with open("table.h", "w") as hdr:
    hdr.write(tableGen.generateHeaderFile())

with open("table.inc", "w") as tbl:
    tbl.write(tableGen.generateTable())
