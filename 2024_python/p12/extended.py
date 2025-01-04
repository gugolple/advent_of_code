import unittest, sys
from collections import deque

# They key is the amount of sides is EQUAL to the amount of corners.
# From that you have to check in a procedural way for all corners.
# They key difficulty was the double corners.
# Due to my layout, I check the 4 corners of any spot, and from there
# you must check the possibility of a double corner in the same area,
# done by testing if both sides (although same letter) has been seen.
# A position is added to seen as first step of the cycle after the
# check if it was seen before.


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

def test_mat_dict(mat_dict, pos, my_letter):
    if pos not in mat_dict or mat_dict[pos] != my_letter:
        return False
    return True

def check_corner(mat_dict, seen, pos, my_let):
    #print("Check corner", pos)
    check_list = []
    cr, cc = pos
    for nr, nc in [(cr-1, cc-1), (cr-1, cc), (cr, cc-1), (cr, cc)]:
        np = (nr, nc)
        #print("Checked corner ref", np)
        check_list.append(test_mat_dict(mat_dict, np, my_let))
    res = 1
    for ncc in non_corner_checks:
        if compare_lists(check_list, ncc):
            res = 0
            break
    # TL and BR
    if compare_lists(check_list, (True, False, False, True)):
        tlp = (cr-1, cc-1)
        brp = pos
        if tlp in seen and brp in seen:
            tl = test_mat_dict(mat_dict, tlp, my_let)
            br = test_mat_dict(mat_dict, brp, my_let)
            print("Double compare tlbr", tl, br)
            if tl and br:
                res = 2
    # TR and BL
    if compare_lists(check_list, (False, True, True, False)):
        trp = (cr-1, cc)
        blp = (cr, cc-1)
        if trp in seen and blp in seen:
            tr = test_mat_dict(mat_dict, trp, my_let)
            bl = test_mat_dict(mat_dict, blp, my_let)
            print("Double compare trbl", tr, bl)
            if tr and bl:
                res = 2
    return res



def check_corners_around(mat_dict, seen, pos):
    cr, cc = pos
    corners_found = list()
    # We move bottom and right, only positive.
    # We check top and left
    for nr, nc in [(cr, cc), (cr, cc+1), (cr+1, cc), (cr+1, cc+1)]:
        np = (nr, nc)
        rcc = check_corner(mat_dict, seen, np, mat_dict[pos])
        if rcc > 0:
            corners_found.append((np, rcc))
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
    corners = dict()
    my_letter = get_pos(mat, start)
    pending = deque([start])
    while len(pending)>0:
        cur = pending.popleft()
        if cur in seen:
            continue
        seen.add(cur)
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
        new_corners = check_corners_around(mat_dict, seen, cur)
        for cor, cv in new_corners:
            if cor not in corners or corners[cor] < cv:
                corners[cor] = cv
        print("walk", cur, get_pos(mat, cur), "new corners", len(new_corners))
        area += 1
        set_pos(mat, cur, '.')
    print(my_letter, area, len(corners))
    print("Matrix dictionary", mat_dict)
    corners_val = sum(corners.values())
    print("Corners", corners_val)
    return area * corners_val 


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
