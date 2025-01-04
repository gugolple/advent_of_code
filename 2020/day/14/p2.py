#!/usr/bin/env python3
import sys, functools
rows = sys.stdin.read().strip().split("\n")

mask_raw = rows[0].split(" = ")[1]
floating_mask = []
or_mask_val = 0

def str_masking(val: int, mask: str):
    t_str = list("{0:b}".format(val))
    val_str = ['0'] * (36 - len(t_str)) + t_str
    print("TV", ''.join(t_str))
    for i,v in enumerate(mask):
        if v == '1' or v == 'X':
            val_str[i] = v
    print(val)
    print(mask.lstrip('0'))
    print(''.join(val_str).strip('0'))
    return ''.join(val_str)


memory = []
for r in rows:
    if "mask" in r:
        mask_raw = r.split(" = ")[1]
    else:
        mem, val = r.split(" = ")
        mem = int(mem.lstrip("mem[").rstrip("]"))
        val = int(val)
        print(mask_raw, mem, val)
        res_mem = str_masking(mem, mask_raw)
        print()
        memory.append((res_mem, val))

def mp_to_pos(inp: str):
    as_list = list(inp)
    res = [as_list[:]]
    for i, v in enumerate(as_list):
        if v == 'X':
            tm = len(res)
            for ti in range(tm):
                lt = res[ti]
                tlt = lt[:]
                lt[i] = '0'
                tlt[i] = '1'
                res.append(tlt)
    return [''.join(i) for i in res]

print("Mem map")
seen = set()
total = 0
for mp, v in reversed(memory):
    print(mp, v)
    pos_mem = mp_to_pos(mp)
    for my_mem in pos_mem:
        if my_mem not in seen:
            total += v
            seen.add(my_mem)

print(total)
