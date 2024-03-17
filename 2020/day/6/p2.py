#!/usr/bin/env python3
import sys
groups = sys.stdin.read().strip().split("\n\n")

total = 0
for group in groups:
    participants = group.split("\n")
    shared_letters = set(list(participants[0]))
    for row in participants[1:]:
        print(row)
        cur_set = set(list(row))
        shared_letters = shared_letters.intersection(cur_set)
    print("shared:", shared_letters)
    total += len(shared_letters)
    print()

print(total)
