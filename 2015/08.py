#!/usr/bin/env python3
import unittest
import sys
import re
from pudb import set_trace

def entry_func(inp_str):
    #set_trace()
    totstr = 0
    totchr = 0
    for l in inp_str.strip().split("\n"):
        l = l.strip()
        ctstr = len(l)
        totstr += ctstr
        cchr = 2
        cidx = l.find('\\', 0)
        while cidx > 0:
            if l[cidx+1] == 'x':
                cchr += 3
            elif l[cidx+1] == '\\':
                cchr += 1
                cidx += 1
            else:
                cchr += 1
            cidx = l.find('\\', cidx+1)
        totchr += ctstr - cchr
        print(ctstr, ctstr - cchr, cchr, l)
    return (totstr, totchr)


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            (r'""', (2, 0)),
            (r'"\\"', (4, 1)),
            (r'"abc"', (5, 3)),
            (r'"aaa\"aaa"', (10, 7)),
            (r'"\x27"', (6, 1)),
            (r'''""
            "abc"
            "aaa\"aaa"
             "\x27"''', (23, 11)),
            (r'"\\aaa\"aaa\x27"', (16, 9)),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
