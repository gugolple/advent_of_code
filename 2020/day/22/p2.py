#!/usr/bin/env python3
import sys
import unittest
import copy
from collections import deque
from functools import reduce


def round(p1, p2):
    vp1 = p1.popleft()
    vp2 = p2.popleft()
    if vp1 > vp2:
        p1.append(vp1)
        p1.append(vp2)
    else:
        p2.append(vp2)
        p2.append(vp1)
    return p1, p2


def recurse(p1, p2, seen):
    print("recurse")
    while len(p1) > 0 and len(p2) > 0:
        tp = (tuple(p1), tuple(p2))
        if tp in seen:
            print("Repeated!")
            return True
        seen.add(tp)
        p1, p2 = round(p1, p2)
    fq = False
    if len(p1) > 0:
        fq = True
    return fq
    # P1 True
    # P2 False


def main(iv):
    seen = set()
    print()
    sid = iv.split("\n\n")
    p1 = deque([int(i) for i in sid[0].split("\n")[1:]])
    p2 = deque([int(i) for i in sid[1].split("\n")[1:]])
    print(p1)
    print(p2)
    repeated = False
    while len(p1) > 0 and len(p2) > 0:
        tp = (tuple(p1), tuple(p2))
        if tp in seen:
            print("Repeated!")
            v1, v2 = p1.popleft(), p2.popleft()
            print("P1")
            p1.append(v1)
            p1.append(v2)
        elif len(p1) > p1[0] and len(p2) > p2[0]:
            print(p1)
            print(p2)
            if recurse(copy.copy(p1), copy.copy(p2), seen):
                v1, v2 = p1.popleft(), p2.popleft()
                print("P1")
                p1.append(v1)
                p1.append(v2)
            else:
                v1, v2 = p1.popleft(), p2.popleft()
                print("P2")
                p2.append(v2)
                p2.append(v1)
        else:
            seen.add(tp)
            p1, p2 = round(p1, p2)

    fq = None
    if len(p1) > 0 or repeated:
        fq = p1
    else:
        fq = p2
    print(f"Winner: {fq}")
    fq = list(reversed(fq))
    total = reduce(lambda x, y: x + y[0] * y[1], enumerate(fq, 1), 0)
    print(f"Winner score: {total}")

    return total


# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):
    def test_basic(self):
        ti = """Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"""
        self.assertEqual(main(ti), 291)

    def test_ind_round_1(self):
        p1 = deque([9, 2, 6, 3, 1])
        p2 = deque([5, 8, 4, 7, 10])
        rp1 = deque([2, 6, 3, 1, 9, 5])
        rp2 = deque([8, 4, 7, 10])
        self.assertEqual(round(p1, p2), (rp1, rp2))


if __name__ == "__main__":
    rows = sys.stdin.read().strip()
    print(main(rows))
