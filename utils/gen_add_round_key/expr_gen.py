import random
import expr as e
from expr import Expr, ExprType
from fixed import UFixed32, UFixed64

def randBool():
    return random.choice([False, True])

class Unique:
    def __init__(self, start = 0):
        self.value = start

    def next(self):
        cur = self.value
        self.value += 1
        return cur

class ExprGen:
    NUM_ROUNDS = 15

    def __init__(self, keys, uniq, state):
        self.keys = keys
        self.uniq = uniq
        self.defs = {}
        self.state = list(map(UFixed32, state))
        self.shftVar = e.var("shft")
        self.roundGuardVars = []
        self.addrGuardVars = {}
        self.root = Expr(ExprType.ROOT, value = [])

    def generate(self):
        for i in range(ExprGen.NUM_ROUNDS):
            var = self.newVar()
            expr = e.define(var, e.xor(e.round, e.const(UFixed32(i))))
            self.seq(expr)
            self.roundGuardVars.append(var)

        for key in self.keys:
            var = self.newVar()
            expr = e.define(
                var,
                e.xor(e.address, e.const(UFixed64(key.address))),
                UFixed64
            )
            self.seq(expr)
            self.addrGuardVars[key.address] = var

        self.seq(e.define(self.shftVar, e.const(UFixed64(0)), UFixed64))

        for i in range(4):
            state = self.stateExpr(i)
            xorConst = UFixed32(random.randrange(0x01000000, 0xffffffff))
            expr = e.xoreq(state, e.const(xorConst))
            self.seq(expr)

        for keyIdx in range(len(self.keys)):
            self.generateForKey(keyIdx)

        return self.root

    def generateForKey(self, keyIdx):
        self.key = self.keys[keyIdx]
        self.order = list(range(ExprGen.NUM_ROUNDS))
        random.shuffle(self.order)
        self.setAddress(UFixed64(self.key.address))

        for eqIdx, curEq in enumerate(map(UFixed32, self.order)):
            self.resetState()
            self.setRound(curEq)
            self.root.calc(self.defs)
            for i in range(len(self.state)):
                if randBool():
                    # const
                    expr, val = self.xorToDest(curEq, i)
                else:
                    # expr
                    expr, val = self.generateXor(i)
                    if expr is None:
                        expr, val = self.xorToDest(i)
                self.setCurState(i, val)

                rightExpr = expr if eqIdx == 0 else self.generateRoundGuard(eqIdx, expr)
                if keyIdx != 0:
                    rightExpr = self.generateAddrGuard(keyIdx, rightExpr)
                state = self.stateExpr(i)
                expr = e.xoreq(state, rightExpr)
                self.seq(expr)

        return self.root

    def newVar(self):
        return e.var(f"a{self.uniq.next()}")

    def seq(self, expr):
        self.root.value.append(expr)

    def stateExpr(self, i):
        return e.var(f"state[{i}]")

    def generateRoundGuard(self, eqIdx, expr):
        guard = None
        for i in range(eqIdx, len(self.order)):
            var = self.roundGuardVars[self.order[i]]
            guard = var if guard is None else e.mul(guard, var)
        if guard is not None:
            cons = random.choice([e.rshifteq, e.lshifteq])
            if expr != self.shftVar:
                self.seq(e.assign(self.shftVar, expr))
            self.seq(cons(self.shftVar, e.guard(guard)))
            return self.shftVar

    def generateAddrGuard(self, keyIdx, expr):
        guard = None
        for i in range(keyIdx, len(self.keys)):
            var = self.addrGuardVars[self.keys[keyIdx].address]
            guard = var if guard is None else e.mul(guard, var)
        if guard is not None:
            #cons = random.choice([e.rshifteq, e.lshifteq])
            cons = e.lshifteq
            if expr != self.shftVar:
                self.seq(e.assign(self.shftVar, expr))
            self.seq(cons(self.shftVar, e.guard(guard)))
            return self.shftVar

    def xorToDest(self, rnd, i):
        dest = self.destState(rnd.value, i)
        const = self.curState(i) ^ dest
        return e.const(const), const

    def setRound(self, rnd):
        self.defs["round"] = e.Var(intType = UFixed32, value = rnd)

    def getRound(self):
        return self.defs["round"].value

    def setAddress(self, address):
        self.defs["address"] = e.Var(intType = UFixed32, value = address)

    def setCurState(self, i, val):
        self.defs["state[%d]" % i] = e.Var(intType = UFixed32, value = val)

    def curState(self, i):
        return self.defs["state[%d]" % i].value

    def resetState(self):
        for i, st in enumerate(self.state):
            self.setCurState(i, st)

    def destState(self, rnd, i):
        return self.state[i] ^ UFixed32(self.key.getNum(rnd, i))

    def generateXor(self, stNum):
        for _ in range(10):
            numOps = random.randrange(2, 5)
            root = None
            for i in range(numOps):
                exprType = random.choice([ExprType.MUL, ExprType.XOR, ExprType.LSHIFT])
                rndrange = (4,15) if exprType is ExprType.LSHIFT else (1000, 1000000)
                lop = e.round if root is None else root
                rop = e.const(UFixed32(random.randrange(*rndrange)))
                root = Expr(exprType, left = lop, right = rop)

            val = root.calc(self.defs)
            if val != UFixed32(0):
                rnd = self.getRound().value
                const = self.destState(rnd, stNum) ^ (self.curState(stNum) ^ val)
                if const != UFixed32(0):
                    root = e.xor(root, e.const(const))
                return root, val
        return None, -1
