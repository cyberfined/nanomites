from fixed import UFixed32, UFixed64
import expr as e
import sys

def runTests(root, keys, state, seed):
    isFailure = False

    for keyIdx, key in enumerate(keys):
        for rnd in range(15):
            defs = {}
            defs["address"] = e.Var(UFixed64, UFixed64(key.address))
            state = list(map(UFixed32, state))
            for i, st in enumerate(state):
                defs["state[%d]" % i] = e.Var(UFixed32, st)
            defs["round"] = e.Var(UFixed32, UFixed32(rnd))
            root.calc(defs)

            for i in range(4):
                exp = state[i] ^ UFixed32(key.getNum(rnd, i))
                act = defs["state[%d]" % i].value
                if act == exp:
                    continue

                isFailure = True
                print("round %d" % rnd, file=sys.stderr)
                print("expected", file=sys.stderr)
                for i in range(4):
                    print("state[%d] = %s" % (i, exp), file=sys.stderr)

                print("\nactual", file=sys.stderr)
                for i in range(4):
                    print(
                        "state[%d] = %s" % (i, defs["state[%d]" % i].value),
                        file=sys.stderr
                    )
                print("", file=sys.stderr)

                break

        if isFailure:
            print("KeyIdx: %d, Seed: %d" % (seed, keyIdx), file=sys.stderr)
            sys.exit(1)
