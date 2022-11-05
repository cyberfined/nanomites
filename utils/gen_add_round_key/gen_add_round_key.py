#!/usr/bin/env python
from round_key import RoundKey
from expr_gen import ExprGen, Unique
from code_gen import CodeGen
from tests import runTests
import argparse
import fileinput
import random
import struct
import sys

parser = argparse.ArgumentParser(description = "Generate whitebox AddRoundKey for AES")
parser.add_argument("-s", dest="seed", metavar="", type=int,
    default=random.randrange(sys.maxsize), help="set random seed")
args = parser.parse_args()

def getKeys():
    inp = sys.stdin.buffer.read()
    keys = []
    for key in struct.iter_unpack("<240BQ", inp):
        keys.append(RoundKey(key[0:len(key)-1], key[len(key)-1]))
    return keys

keys = getKeys()
random.seed(args.seed)
state = [0xa749220, 0xbe93bfa6, 0x852f3eee, 0xde9a4ae0]
exprGen = ExprGen(keys, Unique(), state)
root = exprGen.generate()
runTests(root, keys, state, args.seed)

codeGen = CodeGen(root)
print(codeGen.generate())
