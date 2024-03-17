#!/usr/bin/env python3
import sys, unittest

def rotate(l, n):
    return l[n:] + l[:n]

def main(iv, movs=100, end=True):
    print()
    ring = [int(i) for i in iv]
    cc = 0
    minv = min(ring)
    maxv = max(ring)
    print("Min", minv, "Max", maxv)
    for i in range(movs):
        print()
        print("Iter", ring)
        pv = (cc + 1) % len(ring)
        occ = cc
        rcv = ring[cc]
        print("CC", cc, ring[cc], "RCV", rcv, "PV", pv, ring[pv])
        nv = [(i+pv) % (len(ring)) for i in range(0,3)]
        print("NV", nv)
        l = [ring[i] for i in nv]
        print("Pick ups", l)
        for v in l:
            ring.remove(v)
        print("Ring short", ring)
        cc = ring.index(rcv)
        v = ring[cc] -1
        if v < minv:
            v = maxv
        while v in l:
            if v == minv:
                v = maxv
            else:
                v -= 1
        print("Dest", v)
        ins_idx = ring.index(v) + 1
        print(ins_idx)
        for v in l:
            ring.insert(ins_idx, v)
            ins_idx += 1

        print(ring)
        while ring[occ] != rcv:
            ring = rotate(ring, 1)

        print(ring)
        cc = (occ + 1) % len(ring)
    if end:
        while ring[-1] != 1:
            ring = rotate(ring, 1)
        ring = ring[:-1]
    return ''.join(map(lambda x: str(x), ring))

# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):
    def test_basic_01(self):
        inval = "389125467"
        self.assertEqual(main(inval, 1, False), "328915467")

    def test_basic_02(self):
        inval = "389125467"
        self.assertEqual(main(inval, 2, False), "325467891")

    def test_basic_03(self):
        inval = "389125467"
        self.assertEqual(main(inval, 3, False), "725891346")

    def test_basic_04(self):
        inval = "389125467"
        self.assertEqual(main(inval, 4, False), "325846791")

    def test_basic_05(self):
        inval = "389125467"
        self.assertEqual(main(inval, 5, False), "925841367")

    def test_basic_06(self):
        inval = "389125467"
        self.assertEqual(main(inval, 6, False), "725841936")

    def test_basic_07(self):
        inval = "389125467"
        self.assertEqual(main(inval, 7, False), "836741925")

    def test_basic_08(self):
        inval = "389125467"
        self.assertEqual(main(inval, 8, False), "741583926")

    def test_basic_09(self):
        inval = "389125467"
        self.assertEqual(main(inval, 9, False), "574183926")

    def test_basic_10(self):
        inval = "389125467"
        self.assertEqual(main(inval, 10, False), "583741926")

    def test_basic_t_10(self):
        inval = "389125467"
        self.assertEqual(main(inval, 10), "92658374")

    def test_basic(self):
        inval = "389125467"
        self.assertEqual(main(inval), "67384529")

if __name__ == "__main__":
    rows = sys.stdin.read().strip()
    print(main(rows))
