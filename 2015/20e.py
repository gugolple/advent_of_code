#!/usr/bin/env python3
from operator import mul
import itertools
import functools
import unittest
import sys
import re
import math
from sympy import divisors
from pudb import set_trace

def calculateV(inp):
    LIMV = 50
    divs = divisors(inp)
    divs = [i for i in divs if int(inp/i) < 51]
    return sum(divs) * 11

def midCalc(top, bot):
    return ((top-bot)/2) + bot

def entry_func(inp_str):
    tgt = int(inp_str.strip())
    for i in range(tgt):
        res = calculateV(i)
        if res >= tgt:
            return i
    return None

class TestChallenge(unittest.TestCase):
    def test_v(self):
        testPairs = [
            (51, 781),
        ]
        for inp, res in testPairs:
            t = inp
            print("Tst:", t, res)
            self.assertEqual(calculateV(t), res)

    def test_basic(self):
        testPairs = [
            ('''1365''', 60),
        ]
        for inp, res in testPairs:
            t = inp
            print("Tst:", t, res)
            self.assertEqual(entry_func(t), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
