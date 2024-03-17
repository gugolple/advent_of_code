#!/usr/bin/env python3
import sys
rows = sys.stdin.read().strip().split("\n")
rows = [r.split() for r in rows]

def recurse(idx, acc, seen): 
    nv = 0
    while idx < len(rows):
        print(seen)
        r1 = rows[idx]
        if idx in seen:
            return acc
        print(r1)
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

acc = recurse(0, 0, set())

print(acc)
