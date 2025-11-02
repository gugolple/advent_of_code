#!/usr/bin/env python3
import unittest
import sys
import re
import itertools
from pudb import set_trace

OPERATION = {
    "gain": 1,
    "lose": -1,
}

def depthSearch(edges, persons):
    print(persons)
    print(edges)
    best = None
    for pp in itertools.permutations(sorted(list(persons))):
        th = edges[(pp[0], pp[-1])] + edges[(pp[-1], pp[0])]
        for idx in range(0, len(pp)-1):
            p1, p2 = pp[idx], pp[idx+1]
            th += edges[(p1, p2)] + edges[(p2, p1)]
        if best is None or th > best:
            best = th
        print(pp, th)
    return best

def entry_func(inp_str, d=40):
    #set_trace()
    edges = dict()
    persons = set()
    for l in inp_str.strip().split('\n'):
        src, dst = l.strip()[:-1].split(" happiness units by sitting next to ")
        who, oper = src.split(" would ")
        op, amount = oper.split(" ")
        amount = int(amount)
        #print(who, op, amount, dst)
        edges[(who, dst)] = OPERATION[op] * amount
        persons.add(who)
        persons.add(dst)
    me = "me"
    for p in persons:
        edges[(me, p)] = 0
        edges[(p, me)] = 0
    persons.add(me)
    return depthSearch(edges, persons)

class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ('''Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.''', 330),
        ]
        for inp, res in testPairs:
            print("Tst:", inp, res)
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
