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

valid_bags_pairs = [("shiny gold", 1)]
print(valid_bags_pairs)
total = 0 

while len(valid_bags_pairs) > 0:
    k, a = valid_bags_pairs.pop()
    print(k, a)
    total += int(a)

    nv = bags[k]
    if nv is not None:
        for na, nk in nv:
            valid_bags_pairs.append((nk, int(na) * int(a)))


print("Result!")
print(total-1)
