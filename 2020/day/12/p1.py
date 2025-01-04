#!/usr/bin/env python3
import sys
from enum import Enum
rows = sys.stdin.read().strip().split("\n")

dir_face = 1
pos = [0, 0]
dir_to_pos = {
        "N": 0,
        "E": 1,
        "S": 2,
        "W": 3,
        }

dir_to_mov = [
        (-1,0),
        (0,1),
        (1,0),
        (0,-1),
        ]

rot_r = {
        "90": 1,
        "180": 2,
        "270": 3,
        }

rot_l = {
        "90": 3,
        "180": 2,
        "270": 1,
        }

commands = {
        "N": dir_to_mov[dir_to_pos["N"]],
        "E": dir_to_mov[dir_to_pos["E"]],
        "S": dir_to_mov[dir_to_pos["S"]],
        "W": dir_to_mov[dir_to_pos["W"]],
        }

print(dir_to_mov)
for r in rows:
    print(f"Pos v: {pos[0]} h: {pos[1]} cmd: {r} fac: {dir_face}")
    t = None
    inc = 0
    if r[0] in commands:
        t = commands[r[0]]
    elif r[0] == 'F':
        t = dir_to_mov[dir_face]
    elif r[0] == 'L':
        dir_face = (dir_face + rot_l[r[1:]]) % 4
    else:
        dir_face = (dir_face + rot_r[r[1:]]) % 4
    if t is not None:
        inc = int(r[1:])
        pos[0] += t[0] * inc
        pos[1] += t[1] * inc 

print(f"Final Pos v: {pos[0]} h: {pos[1]} fac: {dir_face}")
pos = [abs(i) for i in pos]
print(sum(pos))
