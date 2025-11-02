#!/usr/bin/env python3
import unittest
import sys
import re
from pudb import set_trace

def entry_func(inp_str, d=40):
    #set_trace()
    cstr = ""
    for l in inp_str.strip().split("\n"):
        cstr = l.strip()
    for _ in range(d):
        nl = []
        print(cstr)
        cchar = None
        ccharc = 0
        for idx, char in enumerate(cstr):
            if char != cchar:
                if cchar is not None:
                    nl.append((ccharc, cchar))
                cchar = char
                ccharc = 1
            else:
                ccharc += 1
        nl.append((ccharc, cchar))
        print(nl)
        cstr = ''.join([str(i) + str(j) for i,j in nl])
    return cstr


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            (('1', 1), "11"),
            (('1', 2), "21"),
            (('1', 3), "1211"),
            (('1', 4), "111221"),
            (('1', 5), "312211"),
            (('11', 1), "21"),
            (('21', 1), "1211"),
            (('1211', 1), "111221"),
            (('111221', 1), "312211"),
        ]
        for inp, res in testPairs:
            print("Tst:", inp, res)
            self.assertEqual(entry_func(inp[0], inp[1]), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(len(entry_func(inp_str)))
