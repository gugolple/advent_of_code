import unittest, sys
from collections import deque

def print_mat(mat):
    for r in mat:
        print(r)

def search_starts(mat):
    for ridx, row in enumerate(mat):
        cur = None
        for cidx, e in enumerate(row):
            if e != cur:
                if e != '.':
                    yield (ridx, cidx)
                cur = e

def set_pos(mat, pos, v):
    mat[pos[0]][pos[1]] = v

def get_pos(mat, pos):
    return mat[pos[0]][pos[1]]

def walk_area(mat, start):
    rows = len(mat)
    cols = len(mat[0])
    area = 0
    per = 0
    seen = set()
    my_letter = get_pos(mat, start)
    pending = deque([start])
    while len(pending)>0:
        cur = pending.popleft()
        if cur in seen:
            continue
        print("walk", cur, get_pos(mat, cur))
        cr, cc = cur
        for nr, nc in [(cr-1, cc), (cr+1, cc), (cr, cc-1), (cr, cc+1)]:
            if nr < 0 or nc < 0 or nr >= rows or nc >= cols:
                per += 1
            elif (nr, nc) in seen:
                continue
            elif get_pos(mat, (nr, nc)) != my_letter:
                per += 1
            elif get_pos(mat, (nr, nc)) != '.':
                pending.append((nr, nc))
        area += 1
        set_pos(mat, cur, '.')
        seen.add(cur)
    print(my_letter, area, per)
    return area * per


def entry_func(inp: str):
    tot = 0
    print()
    mat = [list(i) for i in inp.split("\n")]
    print_mat(mat)
    for ss in search_starts(mat):
        tot += walk_area(mat, ss)
        print_mat(mat)
    print("final")
    print_mat(mat)
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """AAAA
BBCD
BBCC
EEEC"""
        self.assertEqual(entry_func(inp_str), 140)

    def test_example_1(self):
        inp_str = """OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"""
        self.assertEqual(entry_func(inp_str), 772)

    def test_example_2(self):
        inp_str = """RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"""
        self.assertEqual(entry_func(inp_str), 1930)
