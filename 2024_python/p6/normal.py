import unittest, sys
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

def walk(mat, start):
    loc = list(start)
    next_loc = [0,0]
    direction = Direction.up
    walk_step = enum_vals[direction]
    print(loc, direction, walk_step)

    tot = 1
    mat[loc[0]][loc[1]] = 'X'

    while True:
        print(loc, direction, walk_step)
        next_loc[0] = loc[0] + walk_step[0]
        next_loc[1] = loc[1] + walk_step[1]
        if next_loc[0] < 0 or next_loc[1] < 0 or next_loc[0] >= len(mat) or next_loc[1] >= len(mat[0]):
            break
        if mat[next_loc[0]][next_loc[1]] == '#':
            direction = direction.next()
            walk_step = enum_vals[direction]
            next_loc[0] = loc[0]
            next_loc[1] = loc[1]
        elif mat[next_loc[0]][next_loc[1]] == '.':
            mat[next_loc[0]][next_loc[1]] = 'X'
            tot += 1
        loc[0] = next_loc[0]
        loc[1] = next_loc[1]
    return tot 

def entry_func(inp: str):
    tot = 0
    mat = [list(i) for i in inp.split("\n")]
    print(mat)
    start = search_start(mat)
    print(start)
    tot = walk(mat, start)
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
        self.assertEqual(entry_func(inp_str), 41)
