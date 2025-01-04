#!/usr/bin/env python3
import sys
rows = sys.stdin.read().strip().split("\n")
rows = [r.split() for r in rows]

def recurse(idx, acc, seen): 
    nv = 0
    while idx < len(rows):
        r1 = rows[idx]
        if idx in seen:
            return None
        if r1[0] == "nop":
            nv = 1
        elif r1[0] == "acc":
            acc += int(r1[1])
            nv = 1
        elif r1[0] == "jmp":
            t = int(r1[1])
            nv = t
        else:
            print("Invalid input")
            exit(1)
        seen.add(idx)
        idx += nv
    if idx >= len(rows):
        return acc
    return None

for i in range(len(rows)):
    r = rows[i]
    og_r0 = r[0]
    if r[0] == "nop":
        rows[i][0] = "jmp"
    elif r[0] == "jmp":
        rows[i][0] = "nop"
    acc = recurse(0, 0, set())
    if not (acc is None):
        break
    rows[i][0] = og_r0


print(acc)
