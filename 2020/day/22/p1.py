#!/usr/bin/env python3
import sys
import unittest
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


def main(iv):
    print()
    sid = iv.split("\n\n")
    p1 = deque([int(i) for i in sid[0].split("\n")[1:]])
    p2 = deque([int(i) for i in sid[1].split("\n")[1:]])
    print(p1)
    print(p2)
    while len(p1) > 0 and len(p2) > 0:
        p1, p2 = round(p1, p2)

    fq = None
    if len(p1) > 0:
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
        self.assertEqual(main(ti), 306)

    def test_ind_round_1(self):
        p1 = deque([9, 2, 6, 3, 1])
        p2 = deque([5, 8, 4, 7, 10])
        rp1 = deque([2, 6, 3, 1, 9, 5])
        rp2 = deque([8, 4, 7, 10])
        self.assertEqual(round(p1, p2), (rp1, rp2))

    def test_ind_round_2(self):
        p1 = deque([2, 6, 3, 1, 9, 5])
        p2 = deque([8, 4, 7, 10])
        rp1 = deque([6, 3, 1, 9, 5])
        rp2 = deque([4, 7, 10, 8, 2])
        self.assertEqual(round(p1, p2), (rp1, rp2))

    def test_ind_round_3(self):
        p1 = deque([6, 3, 1, 9, 5])
        p2 = deque([4, 7, 10, 8, 2])
        rp1 = deque([3, 1, 9, 5, 6, 4])
        rp2 = deque([7, 10, 8, 2])
        self.assertEqual(round(p1, p2), (rp1, rp2))

    def test_ind_round_4(self):
        p1 = deque([3, 1, 9, 5, 6, 4])
        p2 = deque([7, 10, 8, 2])
        rp1 = deque([1, 9, 5, 6, 4])
        rp2 = deque([10, 8, 2, 7, 3])
        self.assertEqual(round(p1, p2), (rp1, rp2))


if __name__ == "__main__":
    rows = sys.stdin.read().strip()
    print(main(rows))
