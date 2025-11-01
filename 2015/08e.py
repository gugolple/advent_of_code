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
        cchr = 4
        cidx = l.find('\\', 0)
        while cidx > 0:
            if l[cidx+1] == 'x':
                cchr += 1
            elif l[cidx+1] == '\\':
                cchr += 2
                cidx += 1
            else:
                cchr += 2
            cidx = l.find('\\', cidx+1)
        totchr += ctstr + cchr
        print(ctstr + cchr, cchr, ctstr, l)
    return (totstr, totchr)


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            (r'""', (2, 6)),
            (r'"\\"', (4, 10)),
            (r'"abc"', (5, 9)),
            (r'"aaa\"aaa"', (10, 16)),
            (r'"\x27"', (6, 11)),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
