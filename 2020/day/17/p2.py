#!/usr/bin/env python3
import sys, itertools, unittest

def add_tuples(t1, t2):
    return tuple([i+j for i,j in zip(t1,t2)])


def main(rows, iters=6):
    rows  = rows.split("\n")
    # X, Y, Z, W
    active = set()
    print()
    print(rows)
    for ir, r in enumerate(rows):
        for ic, c in enumerate(r):
            if c == '#':
                print(ir, ic)
                active.add((ir, ic, 0, 0))

    possible_movs = [
            (-1,-1,-1,-1),
            (-1,-1,-1,0),
            (-1,-1,-1,1),
            (-1,-1,0,-1),
            (-1,-1,0,0),
            (-1,-1,0,1),
            (-1,-1,1,-1),
            (-1,-1,1,0),
            (-1,-1,1,1),
            (-1,0,-1,-1),
            (-1,0,-1,0),
            (-1,0,-1,1),
            (-1,0,0,-1),
            (-1,0,0,0),
            (-1,0,0,1),
            (-1,0,1,-1),
            (-1,0,1,0),
            (-1,0,1,1),
            (-1,1,-1,-1),
            (-1,1,-1,0),
            (-1,1,-1,1),
            (-1,1,0,-1),
            (-1,1,0,0),
            (-1,1,0,1),
            (-1,1,1,-1),
            (-1,1,1,0),
            (-1,1,1,1),
            (0,-1,-1,-1),
            (0,-1,-1,0),
            (0,-1,-1,1),
            (0,-1,0,-1),
            (0,-1,0,0),
            (0,-1,0,1),
            (0,-1,1,-1),
            (0,-1,1,0),
            (0,-1,1,1),
            (0,0,-1,-1),
            (0,0,-1,0),
            (0,0,-1,1),
            (0,0,0,-1),
            (0,0,0,1),
            (0,0,1,-1),
            (0,0,1,0),
            (0,0,1,1),
            (0,1,-1,-1),
            (0,1,-1,0),
            (0,1,-1,1),
            (0,1,0,-1),
            (0,1,0,0),
            (0,1,0,1),
            (0,1,1,-1),
            (0,1,1,0),
            (0,1,1,1),
            (1,-1,-1,-1),
            (1,-1,-1,0),
            (1,-1,-1,1),
            (1,-1,0,-1),
            (1,-1,0,0),
            (1,-1,0,1),
            (1,-1,1,-1),
            (1,-1,1,0),
            (1,-1,1,1),
            (1,0,-1,-1),
            (1,0,-1,0),
            (1,0,-1,1),
            (1,0,0,-1),
            (1,0,0,0),
            (1,0,0,1),
            (1,0,1,-1),
            (1,0,1,0),
            (1,0,1,1),
            (1,1,-1,-1),
            (1,1,-1,0),
            (1,1,-1,1),
            (1,1,0,-1),
            (1,1,0,0),
            (1,1,0,1),
            (1,1,1,-1),
            (1,1,1,0),
            (1,1,1,1),
            ]
    for _ in range(iters):
        hit_counters = {}
        for cell in active:
            for mov in possible_movs:
                fc = add_tuples(cell, mov)
                if fc in hit_counters:
                    hit_counters[fc]+=1
                else:
                    hit_counters[fc]=1
        print(active, len(active))
        #print(hit_counters)
        print()
        na = set()
        added_next = 0
        # Only if active this turn
        for ac in active:
            hc = 0
            if ac in hit_counters:
                hc = hit_counters[ac]
            if hc == 2:
                added_next += 1
                na.add(ac)
        created_next = 0
        hcn = []
        # Only if 3 hits this turn
        for cl, hc in hit_counters.items():
            while hc >= len(hcn):
                hcn.append(0)
            hcn[hc] += 1
            if hc == 3:
                created_next += 1
                na.add(cl)
        print(len(active), hcn, len(na), added_next, created_next)
        active = na
        print(na)
        print(len(na))
    return len(active)


# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):
    def test_basic_1(self):
        inp = ".#.\n" + \
                "..#\n" + \
                "###"
        self.assertEqual(main(inp,1), 29)

    def test_basic_2(self):
        inp = ".#.\n" + \
                "..#\n" + \
                "###"
        self.assertEqual(main(inp,2), 60)

    def test_basic(self):
        inp = ".#.\n" + \
                "..#\n" + \
                "###"
        self.assertEqual(main(inp), 848)


if __name__ == "__main__":
    rows = sys.stdin.read().strip()
    print(main(rows))
