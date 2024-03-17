#!/usr/bin/env python3
import sys, copy, functools
rows = sys.stdin.read().strip().split("\n")
grid = list(map(lambda x: [*x], rows))
grid_other = copy.deepcopy(grid)

def print_grid(grid: list[list[chr]]):
    for r in grid:
        print(r)

def grid_equals(grid: list[list[chr]], grid_other: list[list[chr]]):
    for lr, rr in zip(grid, grid_other):
        for lc, rc in zip(lr, rr):
            if lc != rc:
                return False
    return True

def count_around(grid: list[list[chr]], position: tuple[int, int], val: chr):
    rl = max(0, position[0]-1)
    rr =  min(len(grid), position[0]+2)
    cl = max(0, position[1]-1)
    cr =  min(len(grid[0]), position[1]+2)
    total = 0
    for ri in range(rl, rr):
        for ci in range(cl, cr):
            if grid[ri][ci] == val:
                total += 1
    #print(rl, rr, "--", cl, cr, "==", total)
    return total

def simulate_step(grid: list[list[chr]], grid_other: list[list[chr]]):
    for ri, r in enumerate(grid):
        for ci, c in enumerate(r):
            ca = count_around(grid, (ri, ci), '#')
            if c == 'L' and ca == 0:
                grid_other[ri][ci] = '#'
            elif c == '#' and ca > 4:
                grid_other[ri][ci] = 'L'
            else:
                grid_other[ri][ci] = grid[ri][ci]


print("initial")
print_grid(grid)
simulate_step(grid, grid_other)

#for i in range(1):
while not grid_equals(grid, grid_other):
    print("Iter")
    t = grid
    grid = grid_other
    grid_other = t
    simulate_step(grid, grid_other)

print("output grid")
print_grid(grid_other)

print(functools.reduce(lambda x,y: x + y.count('#'), grid, 0))

