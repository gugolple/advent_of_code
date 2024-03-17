#!/usr/bin/env python3
import sys
rows = sys.stdin.read().strip().split("\n")
rows = list(map(lambda x: int(x), rows))

# Setup the array
WINDOW_SIZE = 25
SUM_SUBSEC = WINDOW_SIZE-1
ARR_LEN = WINDOW_SIZE * SUM_SUBSEC

def sum_array(window):
    assert(len(window) == WINDOW_SIZE)
    na = []
    for i,r in enumerate(window):
        primary_value = r
        for i2, r2 in enumerate(window[i+1:]):
            na.append(r + r2)
    return na

for i in range(WINDOW_SIZE, len(rows)):
    # We know only positives
    wi = i % WINDOW_SIZE
    w = rows[i-WINDOW_SIZE:i]
    print(w)
    sa = sum_array(w)
    print(sa)
    cn = rows[i]
    if cn not in sa:
        print(cn)
        break
