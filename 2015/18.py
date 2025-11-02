#!/usr/bin/env python3
import functools
import unittest
import sys
import re
from pudb import set_trace

MINV = 0

def printm(mat):
    print('\n'.join([str(r) for r in mat]))
    print()

def getNeighB(MAXV, mat, row, col):
    rowmin = row -1 if row > MINV else MINV
    rowmax = row +1 if row < (MAXV-1) else (MAXV-1)
    colmin = col -1 if col > MINV else MINV
    colmax = col +1 if col < (MAXV-1) else (MAXV-1)
    tot = 0
    for r in range(rowmin, rowmax+1):
        for c in range(colmin, colmax+1):
            if r == row and c == col:
                continue
            tot += mat[r][c]
    #print(rowmin, row, rowmax)
    #print(colmin, col, colmax)
    #print(tot)
    return tot

def entry_func(inp_str, stp=100, MAXV=100):
    dcnt = dict()
    mat = [[0] * (MAXV) for _ in range((MAXV))]
    for row, l in enumerate(inp_str.strip().split('\n')):
        for col, c in enumerate(l.strip()):
            if c == '#':
                mat[row][col] = 1
    printm(mat)
    for itr in range(stp):
        nmat = [[0] * (MAXV) for _ in range((MAXV))]
        for row in range(len(mat)):
            for col in range(len(mat[0])):
                cnt = getNeighB(MAXV, mat, row, col)
                cs = mat[row][col]
                if cnt == 3:
                    nmat[row][col] = 1
                elif cs == 1 and cnt == 2:
                    nmat[row][col] = 1
        mat = nmat
    printm(mat)
    return sum([sum(r) for r in mat])

class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            (('''
              .#.#.#
              ...##.
              #....#
              ..#...
              #.#..#
              ####..''', 1), 11),
            (('''.#.#.#
              ...##.
              #....#
              ..#...
              #.#..#
              ####..''', 2), 8),
            (('''.#.#.#
              ...##.
              #....#
              ..#...
              #.#..#
              ####..''', 3), 4),
            (('''.#.#.#
              ...##.
              #....#
              ..#...
              #.#..#
              ####..''', 4), 4),
        ]
        MAXV = 2
        for inp, res in testPairs:
            t, tgtv = inp
            print("Tst:", t, tgtv, res)
            self.assertEqual(entry_func(t, tgtv, 6), res)
        MAXV = 99

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
