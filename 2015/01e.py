#!/usr/bin/env python3
import unittest
import sys

def entry_func(inp_str) -> int:
    cnt = 0
    for pos, ch in enumerate(inp_str, start=1):
        if ch == ")":
            cnt -= 1
        elif ch == "(":
            cnt += 1
        else:
            raise "Wtf!"
        if cnt < 0:
            return pos


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            (")", 1),
            ("()())", 5),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
