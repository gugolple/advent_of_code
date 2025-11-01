#!/usr/bin/env python3
import unittest
import sys
import re
from pudb import set_trace

def entry_func(inp_str):
    #set_trace()
    tot = 0
    rgxp = re.compile("(.)\\1")
    vowels = ["a", "e", "i", "o", "u"]
    naughty = ["ab", "cd", "pq", "xy"]
    for l in inp_str.strip().split("\n"):
        cv = sum([l.count(v) for v in vowels])
        if cv < 3:
            continue
        if any([n in l for n in naughty]):
            continue
        rr = rgxp.search(l)
        if rr is None:
            continue
        tot += 1
    return tot


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ("ugknbfddgicrmopn", 1),
            ("aaa", 1),
            ("jchzalrnumimnmhp", 0),
            ("haegwjzuvuyypxyu", 0),
            ("dvszwmarrgswjxmb", 0),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
