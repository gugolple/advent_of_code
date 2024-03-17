#!/usr/bin/env python3
import sys
rows = sys.stdin.read().strip().split("\n")
rows = list(map(lambda x: int(x), rows))
rows.append(0)
rows.sort()
rows.append(rows[-1]+3)

print(rows)
MAX_DELTA = 3

# Max difference is 3, so we make it four to index directly
def check_deltas(v: list[int]):
    LMAX_DELTA = MAX_DELTA
    for idx in range(len(v)-1):
        l = v[idx]
        r = v[idx+1]
        if (r - l) > LMAX_DELTA:
            return False
    return True

memo = {}
def combinations(v: list[int], idx: int=0):
    if v[0] != 0:
        return None
    if not check_deltas(v):
        return None
    if (idx +1) == len(v):
        #print(v)
        return 1
    if tuple(v[idx:]) in memo:
        return memo[tuple(v[idx:])]
    # Keep current
    r = combinations(v, idx+1)
    tv = v[:]
    ti = idx+1
    for i in range(3):
        if ti >= len(v):
            break
        tv.remove(tv[idx])
        tr = combinations(tv, ti)
        if tr is None:
            break
        r += tr
    memo[tuple(v[idx:])] = r
    return r



print(combinations(rows))
