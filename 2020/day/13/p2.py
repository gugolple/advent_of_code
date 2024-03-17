#!/usr/bin/env python3
import sys, math
rows = sys.stdin.read().strip().split("\n")

def int_str(string: str):
    res = string
    try:
        res = int(string)
    except:
        pass
    return res


time = int(rows[0])
buses = list(map(int_str, rows[1].split(",")))
print(time, buses)
ids = list(map(lambda x: int(x), filter(lambda x: x != 'x', rows[1].split(","))))
print(ids)
assert(math.gcd(*ids)==1)

print()

def int_proc(t):
    res = t
    if type(t) == int:
        res = time + ( t - (time % t)) 
    return res

rem_times = list(map(int_proc, buses))
print(rem_times)

for i in range(len(rem_times)-1):
    print(i, rem_times[i], rem_times[i+1])
