import unittest, sys, copy
from enum import Enum

def search_start(mat):
    for row_idx in range(len(mat)):
        for cold_idx in range(len(mat[0])):
            if mat[row_idx][cold_idx] == "^":
                return (row_idx, cold_idx)

class Direction(Enum):
    up = 1
    right = 2
    down = 3
    left = 4

    def next(self):
        if self.value == 4:
            return Direction.up
        return Direction(self.value+1)

enum_vals = {
        Direction.up: (-1,0),
        Direction.right: (0,1),
        Direction.down: (1,0),
        Direction.left: (0,-1),
        }

def walk(org_mat, start, direction=Direction.up, depth=0):
    mat = copy.deepcopy(org_mat)
    tot = 0
    loc = list(start)
    next_loc = [0,0]
    direction = direction
    walk_step = enum_vals[direction]
    mat[loc[0]][loc[1]] = [copy.deepcopy(direction)]

    while True:
        next_loc[0] = loc[0] + walk_step[0]
        next_loc[1] = loc[1] + walk_step[1]
        # Bad, we did not loop
        if next_loc[0] < 0 or next_loc[1] < 0 or next_loc[0] >= len(mat) or next_loc[1] >= len(mat[0]):
            break
        #print(loc, direction, walk_step, mat[next_loc[0]][next_loc[1]])
        # Must turn
        if mat[next_loc[0]][next_loc[1]] == '#':
            direction = direction.next()
            walk_step = enum_vals[direction]
            next_loc[0] = loc[0]
            next_loc[1] = loc[1]
        # Mark the step
        elif mat[next_loc[0]][next_loc[1]] == '.':
            mat[next_loc[0]][next_loc[1]] = [copy.deepcopy(direction)]
        # Loop detection
        elif direction in mat[next_loc[0]][next_loc[1]]:
            # Loop detected
            return 1
        else:
            mat[next_loc[0]][next_loc[1]].append(copy.deepcopy(direction))
        loc[0] = next_loc[0]
        loc[1] = next_loc[1]
    return 0 

def entry_func(inp: str):
    tot = 0
    mat = [list(i) for i in inp.split("\n")]
    print(mat)
    start = search_start(mat)
    print(start)

    for r in range(len(mat)):
        print(r, tot)
        for c in range(len(mat[0])):
            if mat[r][c] != '.': continue
            mat[r][c] = '#'
            tot += walk(mat, start)
            mat[r][c] = '.'
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""
        self.assertEqual(entry_func(inp_str), 6)
