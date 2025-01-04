#!/usr/bin/env python3
import sys, re
blocks = sys.stdin.read().strip().split("\n\n")

itm_set = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"}
print(len(itm_set))

valid_blocks = 0
for blk in blocks:
    print("Items:")
    item_lst = re.split(" |\n", blk)
    item_set = set([i.split(":")[0] for i in item_lst])
    dif_set = itm_set.difference(item_set)
    print(dif_set)
    if (len(dif_set) == 0) or (len(dif_set) == 1 and "cid" in dif_set):
        valid_blocks += 1
    print("")

print(valid_blocks)
