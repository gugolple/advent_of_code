#!/usr/bin/env python3
import functools
import unittest
import sys
import re
import numpy as np
from pudb import set_trace

# Sistem of equations
# Capacity = CX * X + CY * Y + ...
# Durability = DX * X + DY * Y + ...
# Flavor = FX * X + FY * Y + ...
# Texture = TX * X + TY * Y + ...
# Total = Capacity * Durability * Flavor * Texture
# Total = (CX * X + CY * Y) * (DX * X + DY * Y) * (FX * X + FY * Y) * (TX * X + TY * Y)

def score(mat, eqv):
    mv = np.array(eqv, np.int64)
    res = np.vecmat(mv, mat)
    ares = int(functools.reduce(lambda x,y: x * y, map(lambda x: x if x>0 else 0, res[:-1])))
    if res[-1] != 500:
        ares = 0
    print(eqv, res, ares)
    return ares

LIM = 101 # 100 + 1 due to range being non inclusive
def recVal(mat, eqv, tused = 0):
    if len(eqv) < (len(mat)-1):
        bv = 0
        for v in range(LIM-tused):
            eqv.append(v)
            cv = recVal(mat, eqv, tused+v)
            eqv.pop()
            if cv > bv:
                bv = cv
        return bv
    eqv.append(LIM - tused - 1)
    bv = score(mat, eqv)
    eqv.pop()
    return bv

def entry_func(inp_str, iters=2503):
    eqs = list()
    for l in inp_str.strip().split('\n'):
        what, specs = l.strip().split(": ")
        lsp = specs.split(", ")
        eq = (what,*[int(i.split(" ")[1]) for i in lsp])
        # what, cap, dur, flav, tex, cal
        print(eq)
        eqs.append(eq)
    mat = np.array([l[1:] for l in eqs], np.int64)
    print(mat)
    bscore = recVal(mat, [])
    return bscore

class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ('''Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3''', 57600000),
        ]
        for inp, res in testPairs:
            t = inp
            print("Tst:", inp, res)
            self.assertEqual(entry_func(t), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
