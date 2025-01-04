#!/usr/bin/env python3
import sys
rows = list(map(lambda x: int(x), sys.stdin.read().strip().split("\n")))

for r1 in rows:
    print(r1)
    for r2 in rows:
        for r3 in rows:
            if r1 != r2 and r2 != r3 and r1 != r3 and (r1 + r2 + r3) == 2020:
                print(r1 * r2 * r3)
                exit(0)
