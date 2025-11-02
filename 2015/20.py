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
    factors = divisors(inp)
    #print(factors)
    return sum(factors) * 10

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
            (1, 10),
            (2, 30),
            (3, 40),
            (4, 70),
            (5, 60),
            (6, 120),
            (7, 80),
            (8, 150),
            (9, 130),
        ]
        for inp, res in testPairs:
            t = inp
            print("Tst:", t, res)
            self.assertEqual(calculateV(t), res)

    def test_basic(self):
        testPairs = [
            ('''10''', 1),
            ('''30''', 2),
            ('''40''', 3),
            ('''70''', 4),
            ('''60''', 4),
            ('''120''', 6),
            ('''80''', 6),
            ('''150''', 8),
            ('''130''', 8),
        ]
        for inp, res in testPairs:
            t = inp
            print("Tst:", t, res)
            self.assertEqual(entry_func(t), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
