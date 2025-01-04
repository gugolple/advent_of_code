#!/usr/bin/env python3
import sys
rows = sys.stdin.read().strip().split("\n")

hidx = 0
hidx_inc = 3

hit_count = 0
for r1 in rows[1:]:
    hidx = (hidx + hidx_inc) % len(rows[0])
    print(hidx, r1[hidx], r1)
    if r1[hidx] == "#":
        hit_count += 1

print(hit_count)
