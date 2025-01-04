#!/usr/bin/env python3
import sys
rows = sys.stdin.read().strip().split("\n")

row_trs = str.maketrans({"B":"1", "F":"0"})
col_trs = str.maketrans({"R":"1", "L":"0"})
mmax = 0
for r1 in rows:
    row = int(r1[:7].translate(row_trs),2)
    col = int(r1[7:].translate(col_trs),2)
    val = row * 8 + col
    print(row, col, val)
    if val > mmax:
        print(val)
        mmax = val
print(mmax)
