#!/usr/bin/env python3
import functools
import unittest
import sys
import re
from pudb import set_trace

def entry_func(inp_str):
    transf, inpstr = inp_str.split("\n\n")
    pos_transf = dict()
    for l in transf.strip().split('\n'):
        src, dst = l.strip().split(" => ")
        if src not in pos_transf:
            pos_transf[src] = list()
        print(src, dst)
        pos_transf[src].append(dst)
    print(pos_transf)
    ansset = set()
    # Iterate over molecule
    inpstr = inpstr.strip()
    print(inpstr)
    for k in pos_transf.keys():
        print("RF", k, inpstr.count(k))
        idx = inpstr.find(k, 0)
        while idx >= 0:
            for pv in pos_transf[k]:
                pm = inpstr[:idx] + pv + inpstr[idx+len(k):]
                #print(pm)
                ansset.add(pm)
            idx = inpstr.find(k, idx+1)
    return len(ansset)

class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ('''
             H => HO
             H => OH
             O => HH

             HOH''', 4),
            ('''
             H => HO
             H => OH
             O => HH

             HOHOHO''', 7),
        ]
        for inp, res in testPairs:
            t = inp
            print("Tst:", t, res)
            self.assertEqual(entry_func(t), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
