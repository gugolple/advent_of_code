#!/usr/bin/env python3
import unittest
import sys
import re
from pudb import set_trace

def entry_func(inp_str):
    #set_trace()
    g = list()
    for y in range(1000):
        r = [False] * 1000
        g.append(r)
    for l in inp_str.strip().split("\n"):
        words = l.split(" ")
        op_range = words[-3:]
        cmd = words[:-3]
        cs = tuple(int(i) for i in op_range[0].split(','))
        ce = tuple(int(i)+1 for i in op_range[2].split(','))
        print(cmd)
        print(op_range)
        print(cs, ce)
        if cmd[0] == "toggle":
            for ridx in range(cs[0], ce[0]):
                for cidx in range(cs[1], ce[1]):
                    g[ridx][cidx] = not g[ridx][cidx]
        else:
            if cmd[1] == "on":
                for ridx in range(cs[0], ce[0]):
                    for cidx in range(cs[1], ce[1]):
                        g[ridx][cidx] = True
            else:
                for ridx in range(cs[0], ce[0]):
                    for cidx in range(cs[1], ce[1]):
                        g[ridx][cidx] = False
    return sum([r.count(True) for r in g])


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ("turn on 0,0 through 999,999", 1000000),
            ("turn on 0,0 through 999,999\ntoggle 0,0 through 999,0", 999000),
            ("turn on 0,0 through 999,999\ntoggle 0,0 through 999,0\nturn off 499,499 through 500,500", 998996),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
