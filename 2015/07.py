#!/usr/bin/env python3
import unittest
import sys
import re
from pudb import set_trace

MAX_VAL = 2**16-1
operations = {
    "AND": lambda x,y: x & y,
    "OR": lambda x,y: x | y,
    "LSHIFT": lambda x,y: (x << y) & MAX_VAL,
    "RSHIFT": lambda x,y: x >> y,
}

def entry_func(inp_str):
    #set_trace()
    w = dict()
    kv = dict()
    pv = dict()
    for l in inp_str.strip().split('\n'):
        l = l.strip()
        op, dst = l.split(" -> ")
        print(l, op, dst)
        if op.isdigit():
            kv[dst] = int(op)
        else:
            pv[dst] = op
    nitd = set()
    while len(pv) > 0:
        for pw in pv.keys():
            op = pv[pw].split(" ")
            print(pw, op)
            if len(op) == 1:
                rh = op[0]
                if rh.isdigit():
                    resv = int(rh)
                elif rh in kv:
                    resv = kv[rh]
                else:
                    continue
                kv[pw] = resv
                nitd.add(pw)
            elif len(op) == 2:
                oper, rh = op
                # Not
                if rh in kv:
                    rhv = kv[rh] | 1<<17
                    resv = (~rhv) & MAX_VAL
                    kv[pw] = resv 
                    nitd.add(pw)
            else:
                lh, oper, rh = op
                if lh.isdigit():
                    lhv = int(lh)
                elif lh in kv:
                    lhv = kv[lh]
                else:
                    continue
                if rh.isdigit():
                    rhv = int(rh)
                elif rh in kv:
                    rhv = kv[rh]
                else:
                    continue
                resv = operations[oper](lhv, rhv)
                kv[pw] = resv
                nitd.add(pw)
        if len(nitd) == 0:
            raise "Not working mate"
        for nd in nitd:
            del pv[nd]
        nitd = set()
    return kv


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ("""123 -> x
                456 -> y
                x AND y -> d
                x OR y -> e
                x LSHIFT 2 -> f
                y RSHIFT 2 -> g
                NOT x -> h
                NOT y -> i""",
             {
                "d": 72,
                "e": 507,
                "f": 492,
                "g": 114,
                "h": 65412,
                "i": 65079,
                "x": 123,
                "y": 456,
            }),
        ]
        for inp, res in testPairs:
            dr = entry_func(inp)
            for k in res.keys():
                print(k, res[k], dr[k])
                self.assertEqual(res[k], dr[k])

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
