#!/usr/bin/env python3
import sys
rows = sys.stdin.read().strip().split("\n")

valid_pass = 0
for r1 in rows:
    times, lett, password = r1.split(" ")
    lett = lett.strip(":")
    times = list(map(lambda x: int(x), times.split("-")))
    hit_count = password.count(lett)
    print(times, lett, password, hit_count)
    if hit_count >= times[0] and hit_count <= times[1]:
        valid_pass += 1

print(valid_pass)
