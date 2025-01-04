#!/usr/bin/env python3
import sys
rows = sys.stdin.read().strip().split("\n")

time = int(rows[0])
buses = list(map(lambda x: int(x), filter(lambda x: x != 'x', rows[1].split(","))))
print(time, buses)

rem_times = [time + ( i - (time % i)) for i in buses]
print(rem_times)

best = min(rem_times)
idx = rem_times.index(best)
bb = buses[idx]
print(best, bb)

print((best - time) * bb)


