#!/usr/bin/env python3
import functools
import unittest
import sys
import re
from pudb import set_trace

seen = set()
def recf(dcnt, lp, kans=list()):
    res = set()
    kkans = tuple(sorted(kans))
    if (kkans, lp) in seen:
        return res
    seen.add((kkans, lp))
    if lp == 0:
        res.add(kkans)
        return res
    if lp < 0:
        return res
    for k in dcnt.keys():
        if dcnt[k] == 0:
            continue
        if k > lp:
            continue
        kans.append(k)
        dcnt[k] -= 1
        res |= recf(dcnt, lp-k, kans)
        dcnt[k] += 1
        kans.pop()
    return res

def entry_func(inp_str, tgtv=150):
    dcnt = dict()
    for l in inp_str.strip().split('\n'):
        v = int(l.strip())
        if v not in dcnt:
            dcnt[v] = 0
        dcnt[v] += 1
    ans = recf(dcnt, tgtv)
    tot = 0
    for a in ans:
        sa = set(a)
        mul = 1
        for ka in sa:
            mv = dcnt[ka] - a.count(ka) + 1
            mul *= mv
        tot += mul
    return tot

class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            (('''20
             15
             10
             5
             5''', 25), 4),
        ]
        for inp, res in testPairs:
            t, tgtv = inp
            print("Tst:", t, tgtv, res)
            self.assertEqual(entry_func(t, tgtv), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
