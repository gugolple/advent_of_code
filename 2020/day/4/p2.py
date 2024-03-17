#!/usr/bin/env python3
import sys, re, string
blocks = sys.stdin.read().strip().split("\n\n")

def val_byr(v):
    t = int(v)
    return t >= 1920 and t <= 2002

def val_iyr(v):
    t = int(v)
    return t >= 2010 and t <= 2020

def val_eyr(v):
    t = int(v)
    return t >= 2020 and t <= 2030

def val_hgt(v):
    if len(v) < 3:
        return False
    h = int(v[:-2])
    t = v[-2:]
    print("HGT", h, t)
    if t == "cm" and h >= 150 and h <= 193:
        return True
    if t == "in" and h >= 59 and h <= 76:
        return True
    return False

def val_hcl(v):
    return v[0] == '#' and (c in string.hexdigits for c in v[1:])

valid_ecl = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
def val_ecl(v):
    return v in valid_ecl

def val_pid(v):
    return len(v) == 9 and v.isdecimal()

def val_cid(v):
    return True

itm_map = {
        "byr": val_byr,
        "iyr": val_iyr,
        "eyr": val_eyr,
        "hgt": val_hgt,
        "hcl": val_hcl,
        "ecl": val_ecl,
        "pid": val_pid,
        "cid": val_cid,
        }

itm_set = set(itm_map.keys())
print(len(itm_set))

valid_blocks = 0
for blk in blocks:
    print("Items:")
    item_lst = map(lambda x: x.split(":"), re.split(" |\n", blk))
    item_map = dict((k, v) for k, v in item_lst)
    item_set = set(item_map.keys())
    dif_set = itm_set.difference(item_set)
    print(dif_set)
    if (len(dif_set) == 0) or (len(dif_set) == 1 and "cid" in dif_set):
        print(item_map)
        res = [itm_map[k](v) for (k, v) in item_map.items()]
        if all(res):
            valid_blocks += 1
    print("")

print(valid_blocks)
