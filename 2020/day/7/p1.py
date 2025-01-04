#!/usr/bin/env python3
import sys, re
rows = sys.stdin.read().strip().split("\n")

bags = {}
for r1 in rows:
    key, dest = r1.split(" bags contain ")
    print(key)
    if "no other" in dest:
        dest = None
    else:
        dest = [tuple(re.sub(r' bag.*', '', i).strip().split(' ', 1)) for i in dest.split(", ")]
    print(dest)
    bags[key] = dest
    print()

last_vb = 0 
valid_bags = ["shiny gold"]
print(bags)
print(valid_bags)

print("Logic!")
while last_vb != len(valid_bags):
    print(last_vb)
    last_vb = len(valid_bags)
    for k, v in bags.items():
        if k not in valid_bags and v is not None and any([cc[1] in valid_bags for cc in v]):
            valid_bags.append(k)

print("Result!")
print(valid_bags)
print(len(valid_bags)-1)
