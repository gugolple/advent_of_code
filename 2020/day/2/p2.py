#!/usr/bin/env python3
import sys
rows = sys.stdin.read().strip().split("\n")

valid_pass = 0
for r1 in rows:
    times, lett, password = r1.split(" ")
    lett = lett.strip(":")
    times = list(map(lambda x: int(x), times.split("-")))
    fp = password[times[0]-1]
    sp = password[times[1]-1]
    print(times, lett, password, fp, sp)
    if not (fp == lett and sp == lett):
        if (fp == lett or sp == lett):
            valid_pass += 1


print(valid_pass)
