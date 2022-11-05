from enum import Enum, auto
from fixed import UFixed32
import operator

class ExprType(Enum):
    ROOT = auto()
    DEFINE = auto()
    ASSIGN = auto()
    CONST = auto()
    VAR = auto()
    AND = auto()
    OR = auto()
    XOR = auto()
    MUL = auto()
    ADD = auto()
    SUB = auto()
    LSHIFT = auto()
    RSHIFT = auto()
    SELFOP = auto()
    GUARD = auto()

class OpInfo:
    def __init__(self, view, operator = None):
        self.join = " " + view + " "
        self.view = view
        self.operator = operator

class Var:
    def __init__(self, intType, value):
        self.intType = intType
        self._value = intType(value.value)

    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, value):
        self._value = self.intType(value)

    def __str__(self):
        return str(self.value)
    
    def __repr__(self):
        return str(self.value)

class Expr:
    operations = {
        ExprType.AND: OpInfo("&", operator.and_),
        ExprType.OR: OpInfo("|", operator.or_),
        ExprType.XOR: OpInfo("^", operator.xor),
        ExprType.MUL: OpInfo("*", operator.mul),
        ExprType.LSHIFT: OpInfo("<<", operator.lshift),
        ExprType.RSHIFT: OpInfo(">>", operator.rshift),
        ExprType.ASSIGN: OpInfo("=")
    }

    def __init__(self, exprType, value=None, intType=None, left=None, right=None):
        self.exprType = exprType
        self.left = left
        self.right = right
        self.value = value
        self.intType = intType

    def calc(self, defs):
        if self.exprType is ExprType.CONST:
            return self.value
        elif self.exprType is ExprType.VAR:
            return self.getDef(defs, self.value).value
        elif self.exprType is ExprType.ASSIGN:
            var = self.getDef(defs, self.left.value)
            var.value = self.right.calc(defs)
            defs[self.left.value] = var
        elif self.exprType is ExprType.ROOT:
            for expr in self.value:
                expr.calc(defs)
        elif self.exprType is ExprType.DEFINE:
            var = Var(intType = self.intType, value = self.right.calc(defs))
            defs[self.left.value] = var
        elif self.exprType is ExprType.GUARD:
            val = self.left.calc(defs)
            return UFixed32(0 if val.value == 0 else 32)
        elif self.exprType is ExprType.SELFOP:
            op = Expr.operations[self.value]
            var = self.getDef(defs, self.left.value)
            rval = self.right.calc(defs)
            var.value = op.operator(var.value, rval)
            defs[self.left.value] = var
        else:
            op = Expr.operations[self.exprType]
            lval = self.left.calc(defs)
            rval = self.right.calc(defs)
            return op.operator(lval, rval)

    def getDef(self, defs, defName):
        val = defs.get(defName, None)
        if val is None:
            raise RuntimeError("Undefined variable %s" % defName)
        return val

    def print(self):
        if self.exprType is ExprType.CONST:
            return str(self.value)
        elif self.exprType is ExprType.VAR:
            return self.value
        elif self.exprType is ExprType.ROOT:
            return "\n".join(expr.print() for expr in self.value)
        elif self.exprType is ExprType.DEFINE:
            args = {"type": str(self.intType),
                    "var": self.left.value,
                    "expr": self.right.print()}
            return "{type} {var} = {expr}".format(**args)
        elif self.exprType is ExprType.SELFOP:
            op = Expr.operations[self.value]
            join = " " + op.view + "= "
            return join.join([self.left.print(), self.right.print()])
        elif self.exprType is ExprType.GUARD:
            return "guard(" + self.left.print() + ")"
        else:
            op = Expr.operations[self.exprType]
            exprs = [self.left.print(), self.right.print()]
            res = op.join.join(exprs)
            if self.exprType is not ExprType.ASSIGN:
                res = "(" + res + ")"
            return res

def define(var, expr, intType = UFixed32):
    return Expr(ExprType.DEFINE, intType = intType, left = var, right = expr)

def const(c):
    return Expr(ExprType.CONST, value = c)

def var(v):
    return Expr(ExprType.VAR, value = v)

def assign(var, expr):
    return Expr(ExprType.ASSIGN, left = var, right = expr)

def and_(left, right):
    return Expr(ExprType.AND, left = left, right = right)

def andeq(left, right):
    return Expr(ExprType.SELFOP, value = ExprType.AND, left = left, right = right)

def or_(left, right):
    return Expr(ExprType.OR, left = left, right = right)

def oreq(left, right):
    return Expr(ExprType.SELFOP, value = ExprType.OR, left = left, right = right)

def xor(left, right):
    return Expr(ExprType.XOR, left = left, right = right)

def xoreq(left, right):
    return Expr(ExprType.SELFOP, value = ExprType.XOR, left = left, right = right)

def mul(left, right):
    return Expr(ExprType.MUL, left = left, right = right)

def muleq(left, right):
    return Expr(ExprType.SELFOP, value = ExprType.MUL, left = left, right = right)

def add(left, right):
    return Expr(ExprType.ADD, left = left, right = right)

def addeq(left, right):
    return Expr(ExprType.SELFOP, value = ExprType.ADD, left = left, right = right)

def sub(left, right):
    return Expr(ExprType.SUB, left = left, right = right)

def subeq(left, right):
    return Expr(ExprType.SELFOP, value = ExprType.SUB, left = left, right = right)

def lshift(left, right):
    return Expr(ExprType.LSHIFT, left = left, right = right)

def lshifteq(left, right):
    return Expr(ExprType.SELFOP, value = ExprType.LSHIFT, left = left, right = right)

def rshift(left, right):
    return Expr(ExprType.RSHIFT, left = left, right = right)

def rshifteq(left, right):
    return Expr(ExprType.SELFOP, value = ExprType.RSHIFT, left = left, right = right)

def guard(x):
    return Expr(ExprType.GUARD, left = x)

round = var("round")

address = var("address")
