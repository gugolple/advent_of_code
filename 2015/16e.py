#!/usr/bin/env python3
import functools
import unittest
import sys
import re
from pudb import set_trace

KV = """children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1"""

DKV = {}
for r in KV.strip().split("\n"):
    k, v = r.split(": ")
    DKV[k] = int(v)

def entry_func(inp_str, iters=2503):
    pos = list()
    for l in inp_str.strip().split('\n'):
        aunt, knowns = l.strip().split(": ", maxsplit=1)
        aunt = int(aunt.removeprefix("Sue "))
        knownsd = dict()
        for k in knowns.split(", "):
            kd, kv = k.split(": ")
            kv = int(kv)
            knownsd[kd] = kv
            dkvv = DKV[kd]
            if kd in ["cats", "trees"]:
                if kv <= dkvv:
                    break
            elif kd in ["pomeranians", "goldfish"]:
                if kv >= dkvv:
                    break
            elif dkvv != kv:
                break
        else:
            pos.append((aunt, knownsd))
    print(pos)
    return pos[0][0]

class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ('''Sue 1: cats: 6, children: 3, perfumes: 0
                Sue 2: pomeranians: 10, cats: 3, vizslas: 5
                Sue 3: cats: 7''', 3),
        ]
        for inp, res in testPairs:
            t = inp
            print("Tst:", inp, res)
            self.assertEqual(entry_func(t), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
