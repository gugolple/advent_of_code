#!/usr/bin/env python3
import sys
rows = list(map(lambda x: int(x), sys.stdin.read().strip().split("\n")))

for r1 in rows:
    print(r1)
    for r2 in rows:
        if r1 != r2 and (r1 + r2) == 2020:
            print(r1 * r2)
            exit(0)
