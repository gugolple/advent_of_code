#!/usr/bin/env python3
import unittest
import sys
#from pudb import set_trace; set_trace()

def entry_func(inp_str) -> int:
    npd = {
        "^": (1, 0),
        ">": (0, 1),
        "v": (-1, 0),
        "<": (0, -1),
    }
    cp = (0, 0)
    sh = set()
    sh.add(cp)
    for l in inp_str.strip().split("\n"):
        for m in l:
            cp = tuple([i + j for i, j in zip(cp, npd[m])])
            sh.add(cp)
    return len(sh)


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            (">", 2),
            ("^>v<", 4),
            ("^v^v^v^v^v", 2),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
