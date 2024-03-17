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

pos = [i for i in range(-1,2)]
directions = []
for i in pos:
    for j in pos:
        directions.append((i, j))
directions.remove((0,0))
print(directions)
def count_around(grid: list[list[chr]], position: tuple[int, int]):
    total = 0
    for ri, ci in directions:
        for inc in range(1, len(grid)):
            cri = ri * inc
            cci = ci * inc
            pr = position[0] + cri
            pc = position[1] + cci
            if not (pr >= 0 and pr < len(grid) and pc >= 0 and pc < len(grid[0])):
                break
            cv = grid[pr][pc]
            #print(f"pr: {pr} pc: {pc} cv: {cv}")
            if cv == 'L':
                break
            if cv == '#':
                total += 1
                break
    return total

def simulate_step(grid: list[list[chr]], grid_other: list[list[chr]]):
    #print("Input")
    #print_grid(grid)
    for ri, r in enumerate(grid):
        for ci, c in enumerate(r):
            #print(f"Val: {c} ri: {ri} ci: {ci}")
            ca = count_around(grid, (ri, ci))
            #print(f"Val: {c} ri: {ri} ci: {ci} ca: {ca}")
            if c == 'L' and ca == 0:
                grid_other[ri][ci] = '#'
            elif c == '#' and ca > 4:
                grid_other[ri][ci] = 'L'
            else:
                grid_other[ri][ci] = grid[ri][ci]
    #print("Output")
    #print_grid(grid_other)


print("initial")
print_grid(grid)
simulate_step(grid, grid_other)

#for i in range(2):
while not grid_equals(grid, grid_other):
    print("Iter")
    t = grid
    grid = grid_other
    grid_other = t
    simulate_step(grid, grid_other)

print("output grid")
print_grid(grid_other)

print(functools.reduce(lambda x,y: x + y.count('#'), grid, 0))

