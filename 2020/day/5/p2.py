#!/usr/bin/env python3
import sys, itertools 
rows = sys.stdin.read().strip().split("\n")

row_trs = str.maketrans({"B":"1", "F":"0"})
col_trs = str.maketrans({"R":"1", "L":"0"})
vals = []
for r1 in rows:
    row = int(r1[:7].translate(row_trs),2)
    col = int(r1[7:].translate(col_trs),2)
    val = row * 8 + col
    print(row, col, val)
    vals.append(val)
vals.sort()
for l, r in itertools.pairwise(vals):
    if r -l > 1:
        print(l+1)
        break
