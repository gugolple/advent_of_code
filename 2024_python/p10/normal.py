import unittest, sys, heapq

def search_starts(mat):
    for ridx, row in enumerate(mat):
        for cidx, e in enumerate(row):
            if e == 0:
                yield (ridx, cidx)

def move_possible(mat, pos, npos):
    vpos = mat[pos[0]][pos[1]]
    vnpos = mat[npos[0]][npos[1]]
    if (vpos+1) == vnpos:
        return True
    return False

def walk_mat(mat, start):
    seen = set()
    hpq = list()
    tot = 0
    walked = True
    hpq.append((-1, start))
    while len(hpq)>0:
        e = heapq.heappop(hpq)
        print(e)
        if e[1] in seen:
            continue
        seen.add(e[1])
        ridx = e[1][0]
        cidx = e[1][1]
        # Just end
        if mat[ridx][cidx] == 9:
            tot += 1
        else:
            if ridx > 0 and move_possible(mat, (ridx, cidx), (ridx-1, cidx)):
                heapq.heappush(hpq, (e[0]-1, (ridx-1, cidx)))
            if ridx < (len(mat)-1) and move_possible(mat, (ridx, cidx), (ridx+1, cidx)):
                heapq.heappush(hpq, (e[0]-1, (ridx+1, cidx)))
            if cidx > 0 and move_possible(mat, (ridx, cidx), (ridx, cidx-1)):
                heapq.heappush(hpq, (e[0]-1, (ridx, cidx-1)))
            if cidx < (len(mat[0])-1) and move_possible(mat, (ridx, cidx), (ridx, cidx+1)):
                heapq.heappush(hpq, (e[0]-1, (ridx, cidx+1)))
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
        self.assertEqual(entry_func(inp_str), 36)

    def test_example_red1(self):
        inp_str = """0123
1234
8765
9876"""
        self.assertEqual(entry_func(inp_str), 1)
