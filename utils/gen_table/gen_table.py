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

    def strType(self):
        if self is ArgType.INT:
            return "long"
        else:
            return "void*"

class CmdType(Enum):
    SYSCALL = 0
    PROC    = 1

    def strType(self):
        if self is CmdType.SYSCALL:
            return "CMD_SYSCALL"
        else:
            return "CMD_PROC"

def Syscall(title, argsTypes, retType):
    return Command(CmdType.SYSCALL, title, argsTypes, retType)

def Proc(title, argsTypes, retType):
    return Command(CmdType.PROC, title, argsTypes, retType)

class Command:
    def __init__(self, cmdType, title, argsTypes, retType):
        self.title = title
        self.cmdType = cmdType
        self.numArgs = len(argsTypes)
        self.args = list(Register)
        random.shuffle(self.args)
        self.args = self.args[:self.numArgs]
        self.argsTypes = argsTypes
        self.retType = retType
        self.number = None

    def inlineFunc(self, secondPass=False):
        argsList = ', '.join([f"(long)a{a.order()}" for a in self.args])

        argsDef = [f"{t.strType()} a{i}" for i, t in enumerate(self.argsTypes)]
        if secondPass:
            argsDef.insert(0, "long num")
        argsDef = ", ".join(argsDef)

        yield f"static inline {self.retType.strType()} nano_{self.title}({argsDef}) {{"
        yield  "    long ret;"

        specialRegs = [Register.R10, Register.R8, Register.R9]
        for r in specialRegs:
            idx = next((i for i, t in enumerate(self.args) if r == t), None)
            if idx == None:
                continue
            yield f"    register long {r} __asm__(\"{r}\") = (long)a{idx};"

        colon = " :" if secondPass else ""
        yield f"    __asm__ __volatile__ (\"int3\\n\\t\"{colon}"
        if not secondPass:
            yield  "                          \"nano_syscall_%=:\\n\\t\" :"
        yield  "                          \"=a\"(ret) :"

        argList = ["\"a\"({})".format("num" if secondPass else 1)]

        for idx, arg in enumerate(self.args):
            if arg in specialRegs:
                argList.append(f"\"{arg.inline()}\"({arg})")
            else:
                argList.append(f"\"{arg.inline()}\"(a{idx})")
        argList = ' '*26 + ', '.join(argList) + " :"
        yield argList
        yield  "                          \"rcx\", \"r11\", \"memory\");"

        yield f"    return ({self.retType.strType()})ret;"
        yield "}"

    def tableEntries(self):
        argsStr = [hex(off.value) for off in self.args]
        argsStr += ['0x00'] * (len(list(Register)) - self.numArgs)
        argsStr = "{" + ','.join(argsStr) + "}"

        yield f"{{.type = {self.cmdType.strType()},"
        if self.cmdType is CmdType.SYSCALL:
            yield f" .number = {self.number},"
        else:
            yield f" .proc_func = {self.title},"
        yield f" .num_args = {self.numArgs},"
        yield f" .args_offsets = {argsStr}"
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
#include <sys/user.h>

#define CMD_SYSCALL 1337
#define CMD_PROC    1338

typedef struct {
    uint32_t type;
    uint16_t num_args;
    uint16_t args_offsets[6];
    union {
        uint16_t number;
        void     *proc_func;
    };
} cmd_entry;

extern cmd_entry cmd_table[];
"""
        return '\n'.join([header, self.generateMacro(), self.generateFuncs()])

    def generateFuncs(self):
        lines = ["#ifndef SECOND_PASS"]
        for idx, cmd in enumerate(self.commands):
            for line in cmd.inlineFunc():
                lines.append(line)
            if idx != len(self.commands)-1:
                lines.append("")
        lines.append("#else")
        for idx, cmd in enumerate(self.commands):
            for line in cmd.inlineFunc(True):
                lines.append(line)
            if idx != len(self.commands)-1:
                lines.append("")
        lines.append("#endif\n")
        return '\n'.join(lines)

    def generateMacro(self):
        lines = []
        maxTitleLen = max(len(cmd.title) for cmd in self.commands)
        for idx, cmd in enumerate(self.commands):
            spaces = ' ' * (maxTitleLen - len(cmd.title) + 1)
            lines.append(f"#define NANO_{cmd.title.upper()}_OFFSET{spaces}{idx}")
        lines.append("")
        return '\n'.join(lines)

    def generateTable(self):
        table = ["cmd_entry cmd_table[] = {"]
        for cmd in self.commands:
            for entry in cmd.tableEntries():
                table.append("    " + entry)
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
