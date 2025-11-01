#!/usr/bin/env python3
import unittest
import sys
#from pudb import set_trace; set_trace()

def entry_func(inp_str) -> int:
    tot = 0
    for l in inp_str.strip().split("\n"):
        l, w, h = [int(i) for i in l.split('x')]
        lst = [l, w, h]
        lst.remove(max(lst))
        tot += sum(i*2 for i in lst) + l*w*h
    return tot


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ("2x3x4", 34),
            ("1x1x10", 14),
            ("2x3x4\n1x1x10", 48),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
