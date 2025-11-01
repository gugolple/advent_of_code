#!/usr/bin/env python3
import unittest
import sys
import re
from pudb import set_trace

def entry_func(inp_str):
    #set_trace()
    tot = 0
    rgxp = re.compile("(..).*\\1")
    rgxp1 = re.compile("(.).\\1")
    for l in inp_str.strip().split("\n"):
        rr = rgxp.search(l)
        if rr is None:
            continue
        rr = rgxp1.search(l)
        if rr is None:
            continue
        tot += 1
    return tot


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ("qjhvhtzxzqqjkmpb", 1),
            ("xxyxx", 1),
            ("uurcxstgmygtbstg", 0),
            ("ieodomkazucvgmuy", 0),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
