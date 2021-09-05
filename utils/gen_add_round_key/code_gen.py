import expr as e

class CodeGen:
    def __init__(self, root):
        self.root = root

    def generate(self):
        if self.root.exprType is not e.ExprType.ROOT:
            raise ValueError("expression must have root type")

        guardCode = """\
static inline uint32_t sgn(uint32_t n) {
    return (-n>>31)-(n>>31);
}

static inline uint32_t guard(uint32_t n) {
    n += sgn(n)&(1 - sgn(n&63));
    uint32_t b = (n & 63) >> 5;
    return (n*(63 + 3906*b)) & 63;
}
"""
        code = [guardCode]
        code.append("static inline void add_round_key(state_t _state, uint32_t round) {")
        code.append("    uint32_t *state = (uint32_t*)_state;")

        for expr in self.root.value:
            code.append("    " + expr.print() + ";")

        code.append("}\n")
        return "\n".join(code)
