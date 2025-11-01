#!/usr/bin/env python3
import unittest
import sys

def entry_func(inp_str) -> int:
    return inp_str.count("(") - inp_str.count(")")


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ("(())", 0),
            ("()()", 0),
            ("(((", 3),
            ("(()(()(", 3),
            ("))(((((", 3),
            ("())", -1),
            ("))(", -1),
            (")))", -3),
            (")())())", -3),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
