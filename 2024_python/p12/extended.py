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

def compare_lists(l1, l2):
    for e1, e2 in zip(l1, l2):
        if e1 != e2:
            return False
    return True

non_corner_checks = [
        (False, True, False, True),
        (True, False, True, False),
        (True, True, False, False),
        (False, False, True, True),
        (True, True, True, True),
        (False, False, False, False)
        ]
def check_corner(mat_dict, pos, my_let):
    #print("Check corner", pos)
    check_list = []
    cr, cc = pos
    for nr, nc in [(cr-1, cc-1), (cr-1, cc), (cr, cc-1), (cr, cc)]:
        np = (nr, nc)
        #print("Checked corner ref", np)
        if np not in mat_dict or mat_dict[np] != my_let:
            check_list.append(False)
        else:
            check_list.append(True)
    res = True
    for ncc in non_corner_checks:
        if compare_lists(check_list, ncc):
            res = False
            break
    return res



def check_corners_around(mat_dict, corners_seen, pos):
    cr, cc = pos
    corners_found = set()
    # We move bottom and right, only positive.
    # We check top and left
    for nr, nc in [(cr, cc), (cr, cc+1), (cr+1, cc), (cr+1, cc+1)]:
        np = (nr, nc)
        if np in corners_seen:
            continue
        corners_seen.add(np)
        if check_corner(mat_dict, np, mat_dict[pos]):
            corners_found.add(np)
    return corners_found


def walk_area(mat, start):
    rows = len(mat)
    cols = len(mat[0])
    mat_dict = dict()
    for ridx, row in enumerate(mat):
        for cidx, e in enumerate(row):
            mat_dict[(ridx, cidx)] = e
    area = 0
    seen = set()
    corners = set()
    corners_seen = set()
    my_letter = get_pos(mat, start)
    pending = deque([start])
    while len(pending)>0:
        cur = pending.popleft()
        if cur in seen:
            continue
        cr, cc = cur
        for nr, nc in [(cr-1, cc), (cr+1, cc), (cr, cc-1), (cr, cc+1)]:
            if nr < 0 or nc < 0 or nr >= rows or nc >= cols:
                continue
            elif (nr, nc) in seen:
                continue
            elif get_pos(mat, (nr, nc)) != my_letter:
                continue
            elif get_pos(mat, (nr, nc)) != '.':
                pending.append((nr, nc))
        new_corners = check_corners_around(mat_dict, corners_seen, cur)
        for cor in new_corners:
            corners.add(cor)
        print("walk", cur, get_pos(mat, cur), "new corners", len(new_corners))
        area += 1
        set_pos(mat, cur, '.')
        seen.add(cur)
    print(my_letter, area, len(corners))
    print("Matrix dictionary", mat_dict)
    print("Corners seen", corners_seen)
    return area * len(corners)


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
#    def test_example(self):
#        inp_str = """AAAA
#BBCD
#BBCC
#EEEC"""
#        self.assertEqual(entry_func(inp_str), 80)
#
#    def test_example_1(self):
#        inp_str = """OOOOO
#OXOXO
#OOOOO
#OXOXO
#OOOOO"""
#        self.assertEqual(entry_func(inp_str), 436)
#
#    def test_example_3(self):
#        inp_str = """EEEEE
#EXXXX
#EEEEE
#EXXXX
#EEEEE"""
#        self.assertEqual(entry_func(inp_str), 236)
#
    def test_example_3(self):
        inp_str = """AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"""
        self.assertEqual(entry_func(inp_str), 368)
#
#    def test_example_2(self):
#        inp_str = """RRRRIICCFF
#RRRRIICCCF
#VVRRRCCFFF
#VVRCCCJFFF
#VVVVCJJCFE
#VVIVCCJJEE
#VVIIICJJEE
#MIIIIIJJEE
#MIIISIJEEE
#MMMISSJEEE"""
#        self.assertEqual(entry_func(inp_str), 1206)
