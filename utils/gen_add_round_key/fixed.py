import ctypes
import operator

class UFixedType:
    def __init__(self, cons, view):
        self.cons = cons
        self.view = view

    def __call__(self, value):
        return UFixed(self.cons, value)

    def __str__(self):
        return self.view

    def __repr__(self):
        return self.view

UFixed64 = UFixedType(ctypes.c_uint64, "uint64_t")

UFixed32 = UFixedType(ctypes.c_uint32, "uint32_t")

class UFixed:
    def __init__(self, cons, value):
        self._value = cons(value) if type(value) is int else cons(value.value)
        self.size = ctypes.sizeof(self._value)
        self.cons = cons

    @property
    def value(self):
        return self._value.value

    def __eq__(self, right): return self.value == right.value
    def __ne__(self, right): return self.value != right.value
    def __add__(self, right): return self.__binOp__(operator.add, right)
    def __sub__(self, right): return self.__binOp__(operator.sub, right)
    def __mul__(self, right): return self.__binOp__(operator.mul, right)
    def __and__(self, right): return self.__binOp__(operator.and_, right)
    def __or__(self, right): return self.__binOp__(operator.or_, right)
    def __xor__(self, right): return self.__binOp__(operator.xor, right)

    def __rshift__(self, right):
        bigger = self if self.size >= right.size else right
        cons = bigger.cons
        mask = bigger.size*8-1
        return UFixed(cons, self.value >> (right.value & mask))

    def __lshift__(self, right):
        bigger = self if self.size >= right.size else right
        cons = bigger.cons
        mask = bigger.size*8-1
        return UFixed(cons, self.value << (right.value & mask))

    def __binOp__(self, func, right):
        cons = self.cons if self.size >= right.size else right.cons
        return UFixed(cons, func(self.value, right.value))

    def __int__(self):
        return self.value

    def __str__(self):
        return hex(self.value)

    def __repr__(self):
        return hex(self.value)
