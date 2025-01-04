#!/usr/bin/env python3
import sys, functools
rows = sys.stdin.read().strip().split("\n")

mask_raw = rows[0].split(" = ")[1]
or_mask_val = 0
and_mask_val = 0

memory = dict()
for r in rows:
    if "mask" in r:
        mask_raw = r.split(" = ")[1]
        ml = len(mask_raw)-1
        print(mask_raw, ml)
        or_mask = list(filter(lambda x: x[1] == '1', enumerate(mask_raw)))
        or_mask_val = functools.reduce(lambda x,y: x | 1<<(ml-y[0]), or_mask, 0)
        print(or_mask, or_mask_val)
        and_mask = list(filter(lambda x: x[1] == '0', enumerate(mask_raw)))
        and_mask_val = ~functools.reduce(lambda x,y: x | 1<<(ml-y[0]), and_mask, 0)
        print(and_mask, and_mask_val)
    else:
        mem, val = r.split(" = ")
        mem = int(mem.lstrip("mem[").rstrip("]"))
        val = int(val)
        val &= and_mask_val
        val |= or_mask_val
        #print(mem, val)
        memory[mem] = val

print(sum(memory.values()))
