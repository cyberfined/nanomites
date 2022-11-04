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
    def __init__(self, key, uniq, order):
        self.key = key
        self.uniq = uniq
        self.order = order
        self.defs = {}
        self.state = list(map(UFixed32, [0xa749220, 0xbe93bfa6, 0x852f3eee, 0xde9a4ae0]))
        self.shftVar = e.var("shft")
        self.guardVars = []
        self.root = Expr(ExprType.ROOT, value = [])

    def generate(self):
        for i in range(15):
            var = self.newVar()
            expr = e.define(var, e.xor(e.round, e.const(UFixed32(i))))
            self.seq(expr)
            self.guardVars.append(var)
        self.seq(e.define(self.shftVar, e.const(UFixed64(0)), UFixed64))

        for i in range(4):
            state = self.stateExpr(i)
            xorConst = UFixed32(random.randrange(0x01000000, 0xffffffff))
            expr = e.xoreq(state, e.const(xorConst))
            self.seq(expr)

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

                rightExpr = expr if eqIdx == 0 else self.generateGuard(eqIdx, expr)
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

    def generateGuard(self, eqIdx, expr):
        guard = None
        for i in range(eqIdx, len(self.order)):
            var = self.guardVars[self.order[i]]
            guard = var if guard is None else e.mul(guard, var)
        if guard is not None:
            cons = random.choice([e.rshifteq, e.lshifteq])
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
