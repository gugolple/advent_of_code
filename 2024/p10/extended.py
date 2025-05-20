import unittest, sys, heapq

def search_starts(mat):
    for ridx, row in enumerate(mat):
        for cidx, e in enumerate(row):
            if e == 0:
                yield (ridx, cidx)

def move_possible(mat, pos, npos):
    vpos = mat[pos[0]][pos[1]]
    vnpos = mat[npos[0]][npos[1]]
    if vnpos == '.':
        return False
    if (vpos+1) == vnpos:
        return True
    return False

def walk_mat_rec(mat, seen, node):
    print(node)
    if node in seen:
        print("seen", seen[node])
        return seen[node]
    ridx = node[0]
    cidx = node[1]
    # Just end
    tot = 0
    if mat[ridx][cidx] == 9:
        tot += 1
    else:
        if ridx > 0 and move_possible(mat, (ridx, cidx), (ridx-1, cidx)):
            tot += walk_mat_rec(mat, seen, (ridx-1, cidx))
        if ridx < (len(mat)-1) and move_possible(mat, (ridx, cidx), (ridx+1, cidx)):
            tot += walk_mat_rec(mat, seen, (ridx+1, cidx))
        if cidx > 0 and move_possible(mat, (ridx, cidx), (ridx, cidx-1)):
            tot += walk_mat_rec(mat, seen, (ridx, cidx-1))
        if cidx < (len(mat[0])-1) and move_possible(mat, (ridx, cidx), (ridx, cidx+1)):
            tot += walk_mat_rec(mat, seen, (ridx, cidx+1))
    seen[node] = tot
    return tot

def walk_mat(mat, start):
    seen = dict()
    tot = walk_mat_rec(mat, seen, start)
    return tot

def entry_func(inp: str):
    tot = 0
    print(inp)
    mat = [[int(i) for i in l] for l in inp.split("\n")]
    print(mat)
    for start in search_starts(mat):
        print()
        print(start)
        tot += walk_mat(mat, start)
        # Print 1 line per
        #for r in op_mat:
        #    print(r)
        #print()
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"""
        self.assertEqual(entry_func(inp_str), 81)

#    def test_example_red1(self):
#        inp_str = """.....0.
#..4321.
#..5..2.
#..6543.
#..7..4.
#..8765.
#..9...."""
#        self.assertEqual(entry_func(inp_str), 3)
#
#    def test_example_red2(self):
#        inp_str = """..90..9
#...1.98
#...2..7
#6543456
#765.987
#876....
#987...."""
#        self.assertEqual(entry_func(inp_str), 13)

    def test_example_red2(self):
        inp_str = """012345
123456
234567
345678
486789
567898"""
        self.assertEqual(entry_func(inp_str), 227)
