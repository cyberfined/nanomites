from fixed import UFixed32
import expr as e
import sys

def runTests(root, gen, seed):
    isFailure = False

    for rnd in range(15):
        defs = {}
        for i, st in enumerate(gen.state):
            defs["state[%d]" % i] = e.Var(UFixed32, st)
        defs["round"] = e.Var(UFixed32, UFixed32(rnd))
        root.calc(defs)

        for i in range(4):
            exp = gen.destState(rnd, i)
            act = defs["state[%d]" % i].value
            if act == exp:
                continue

            isFailure = True
            print("round %d" % rnd, file=sys.stderr)
            print("expected", file=sys.stderr)
            for i in range(4):
                print("state[%d] = %s" % (i, gen.destState(rnd, i)), file=sys.stderr)

            print("\nactual", file=sys.stderr)
            for i in range(4):
                print("state[%d] = %s" % (i, defs["state[%d]" % i].value), file=sys.stderr)
            print("", file=sys.stderr)

            break

    if isFailure:
        print("Seed: %d" % seed, file=sys.stderr)
        sys.exit(1)
