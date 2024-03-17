#!/usr/bin/env python3
import sys, unittest
from collections import deque
from functools import reduce

def main(iv):
    print()
    sid = iv.split("\n\n")
    p1 = deque([int(i) for i in sid[0].split("\n")[1:]])
    p2 = deque([int(i) for i in sid[1].split("\n")[1:]])
    print(p1)
    print(p2)
    while len(p1) > 0 or len(p2) > 0:
        vpi1 = None
        vp2 = None
        try:
            vp1 = p1.popleft()
        except:
            break
        try:
            vp2 = p2.popleft()
        except:
            break
        if vp1 > vp2:
            p1.append(vp1)
            p1.append(vp2)
        else:
            p2.append(vp2)
            p2.append(vp1)

    fq = None
    if len(p1) > 0:
        fq = p1
    else:
        fq = p2
    print(fq)
    fq = list(reversed(fq))
    print(fq)

    total = reduce(lambda x,y: x + y[0] * y[1], enumerate(fq,1), 0)

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

if __name__ == "__main__":
    rows = sys.stdin.read().strip()
    print(main(rows))
