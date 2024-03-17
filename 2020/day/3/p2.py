#!/usr/bin/env python3
import sys
from functools import reduce
rows = sys.stdin.read().strip().split("\n")


hits = []
paths = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
for hidx_inc, d in paths:
    hit_count = 0
    hidx = 0
    for ridx in range(d, len(rows), d):
        r1 = rows[ridx]
        hidx = (hidx + hidx_inc) % len(rows[0])
        print(hidx, r1[hidx], r1)
        if r1[hidx] == "#":
            hit_count += 1
    hits.append(hit_count)

print(hits)
print(reduce(lambda x, y: x*y, hits))
