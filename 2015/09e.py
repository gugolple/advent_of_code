#!/usr/bin/env python3
import unittest
import sys
import re
from pudb import set_trace

def shortestRec(graphCities, ccit, ccost, seencit, pendcit, visitedOrd):
    if len(pendcit) == 0:
        print(ccost, visitedOrd)
        return ccost
    bres = None
    for pc in pendcit:
        if pc in seencit:
            continue
        seencit.add(pc)
        pendcit.remove(pc)
        visitedOrd.append(pc)
        cres = shortestRec(graphCities, pc, ccost + graphCities[ccit][pc], seencit, pendcit, visitedOrd)
        if bres is None or cres > bres:
            bres = cres
        visitedOrd.pop()
        pendcit.add(pc)
        seencit.remove(pc)
    return bres

def shortest(graphCities):
    bres = None
    for cit in graphCities.keys():
        pcit = set(graphCities.keys())
        pcit.remove(cit)
        scit = set()
        scit.add(cit)
        vo = list()
        vo.append(cit)
        cres = shortestRec(graphCities, cit, 0, scit, pcit, vo)
        if bres is None or cres > bres:
            bres = cres
    return bres

def entry_func(inp_str):
    #set_trace()
    cities = dict()
    for l in inp_str.strip().split("\n"):
        edge, v = l.strip().split(" = ")
        v = int(v)
        org, dst = edge.split(" to ")
        if org not in cities:
            cities[org] = dict()
        cities[org][dst] = v
        if dst not in cities:
            cities[dst] = dict()
        cities[dst][org] = v
    print(cities)
    res = shortest(cities)
    return res 


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ("""London to Dublin = 464
                London to Belfast = 518
                Dublin to Belfast = 141""",
             982),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
