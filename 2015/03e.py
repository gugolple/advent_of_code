#!/usr/bin/env python3
import unittest
import sys
#from pudb import set_trace; set_trace()

def addTuples(tp1, tp2):
    return tuple([i + j for i, j in zip(tp1, tp2)])

def entry_func(inp_str) -> int:
    npd = {
        "^": (1, 0),
        ">": (0, 1),
        "v": (-1, 0),
        "<": (0, -1),
    }
    cp1 = (0, 0)
    cp2 = (0, 0)
    sh = set()
    sh.add(cp1)
    nm = 0
    for l in inp_str.strip().split("\n"):
        for m in l:
            ncp = None
            if nm == 0:
                cp1 = addTuples(cp1, npd[m])
                ncp = cp1
                nm = 1
            else:
                cp2 = addTuples(cp2, npd[m])
                ncp = cp2
                nm = 0
            sh.add(ncp)
    return len(sh)


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            (">", 2),
            ("^v", 3),
            ("^>v<", 3),
            ("^v^v^v^v^v", 11),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
