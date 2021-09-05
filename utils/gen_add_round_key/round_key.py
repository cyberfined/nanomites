class RoundKey:
    def __init__(self, roundKey = None):
        self.roundKey = roundKey

    def getNum(self, rnd, j):
        start = 16*rnd + j*4
        bts = self.roundKey[start:start+4]
        return (bts[3] << 24) | (bts[2] << 16) | (bts[1] << 8) | bts[0]
