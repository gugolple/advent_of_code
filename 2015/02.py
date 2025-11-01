#!/usr/bin/env python3
import unittest
import sys
#from pudb import set_trace; set_trace()

def entry_func(inp_str) -> int:
    tot = 0
    for l in inp_str.strip().split("\n"):
        l, w, h = [int(i) for i in l.split('x')]
        areas = [2*l*w, 2*w*h, 2*h*l]
        tot += sum(areas) + min(areas)/2
    return tot


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ("2x3x4", 58),
            ("1x1x10", 43),
            ("2x3x4\n1x1x10", 58+43),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
