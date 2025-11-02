#!/usr/bin/env python3
import functools
import unittest
import sys
import re
from pudb import set_trace

def recv(inv_transf_l, inv_transf, curstr, seen: set, d=0):
    if curstr in inv_transf_l:
        print("ANS!", d+1, curstr)
        return d+1
    if curstr in seen:
        return None
    seen.add(curstr)
    best = None
    for k in inv_transf:
        idx = curstr.find(k, 0)
        while idx >= 0:
            for pt in inv_transf[k]:
                pm = curstr[:idx] + pt + curstr[idx+len(k):]
                cs = recv(inv_transf_l, inv_transf, pm, seen, d+1)
                if cs is None:
                    continue
                if best is None or best > cs:
                    best = cs
            idx = curstr.find(k, idx+1)
    return best


def entry_func(inp_str):
    transf, inpstr = inp_str.split("\n\n")
    pos_transf = dict()
    inv_transf = dict()
    # Only last step, to enfornce no shenanigans
    inv_transf_l = dict()
    for l in transf.strip().split('\n'):
        src, dst = l.strip().split(" => ")
        if src not in pos_transf:
            pos_transf[src] = list()
        pos_transf[src].append(dst)
        if src == 'e':
            if dst not in inv_transf_l:
                inv_transf_l[dst] = list()
            inv_transf_l[dst].append(src)
        else:
            if dst not in inv_transf:
                inv_transf[dst] = list()
            inv_transf[dst].append(src)
    print(pos_transf)
    print(inv_transf)
    ansset = set()
    # Iterate over molecule
    inpstr = inpstr.strip()
    print(inpstr)
    return recv(inv_transf_l, inv_transf, inpstr, set(), 0)

class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ('''
            e => H
            e => O
            H => HO
            H => OH
            O => HH

             HOH''', 3),
            ('''
            e => H
            e => O
            H => HO
            H => OH
            O => HH

             HOHOHO''', 6),
        ]
        for inp, res in testPairs:
            t = inp
            print("Tst:", t, res)
            self.assertEqual(entry_func(t), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
