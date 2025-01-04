#!/usr/bin/env python3
import sys
rows = sys.stdin.read().strip().split("\n")
rows = list(map(lambda x: int(x), rows))
rows.append(0)
rows.sort()
rows.append(rows[-1]+3)

# Max difference is 3, so we make it four to index directly
deltas = [0] * 4
for idx in range(len(rows)-1):
    l = rows[idx]
    r = rows[idx+1]
    print(l, r)
    deltas[r - l] += 1

print(deltas)
print(deltas[3] * deltas[1])
