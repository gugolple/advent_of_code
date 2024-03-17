#!/usr/bin/env python3
import sys
rows = sys.stdin.read().strip().split("\n")
rows = list(map(lambda x: int(x), rows))

# Setup the array
WINDOW_SIZE = 25
SUM_SUBSEC = WINDOW_SIZE-1
ARR_LEN = WINDOW_SIZE * SUM_SUBSEC

def sum_array(window):
    assert(len(window) == WINDOW_SIZE)
    na = []
    for i,r in enumerate(window):
        primary_value = r
        for i2, r2 in enumerate(window[i+1:]):
            na.append(r + r2)
    return na

sol_idx = -1
sol_num = -1
for i in range(WINDOW_SIZE, len(rows)):
    # We know only positives
    wi = i % WINDOW_SIZE
    w = rows[i-WINDOW_SIZE:i]
    sa = sum_array(w)
    cn = rows[i]
    if cn not in sa:
        sol_num = cn
        sol_idx = i
        break

print(sol_num)

roll_sum = 0
vec_roll_sum = []
vec_roll_sum.append(roll_sum)
for i in range(sol_idx):
    roll_sum += rows[i]
    vec_roll_sum.append(roll_sum)

print(vec_roll_sum)
for i in range(len(vec_roll_sum)):
    for j in range(i, len(vec_roll_sum)):
        if (j - i) > 0 and (vec_roll_sum[j] - vec_roll_sum[i]) == sol_num:
            print(i, j)
            rw = rows[i:j]
            print(rw)
            mn = min(rw)
            mm = max(rw)
            print(mn, mm)
            print(mn + mm)
            exit(0)
