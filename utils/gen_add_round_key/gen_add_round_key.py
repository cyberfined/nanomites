#!/usr/bin/env python
from round_key import RoundKey
from expr_gen import ExprGen, Unique
from code_gen import CodeGen
from tests import runTests
import sys
import random
import argparse

parser = argparse.ArgumentParser(description = "Generate whitebox AddRoundKey for AES")
parser.add_argument("-f", dest="input", metavar="",
    help="file with aes key, stdin is default")
parser.add_argument("-o", dest="output", metavar="",
    help="output file, stdout is default")
parser.add_argument("-s", dest="seed", metavar="", type=int,
    default=random.randrange(sys.maxsize), help="set random seed")
args = parser.parse_args()

def getKey():
    if args.input is not None:
        with open(args.input, "rb") as inp:
            key = inp.read(240)
    else:
        key = sys.stdin.buffer.read(240)

    if len(key) != 240:
        print("Error: key length must be 240 bytes long", file=sys.stderr)
        sys.exit(1)
    return RoundKey(key)

key = getKey()
output = sys.stdout if args.output is None else open(args.output, "w")
random.seed(args.seed)
order = list(range(15))
random.shuffle(order)

exprGen = ExprGen(key, Unique(), order)
root = exprGen.generate()
runTests(root, exprGen, args.seed)

codeGen = CodeGen(root)
output.write(codeGen.generate())
output.close()
