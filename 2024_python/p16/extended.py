import unittest, sys, heapq
from copy import deepcopy
from collections import deque
from enum import Enum
sys.setrecursionlimit(15000)

# Orientation
# Just for me
#    N
#  W # E
#    S
class Or(Enum):
    N = 1# North
    E = 2# East
    S = 3# South
    W = 4# West
    I = 5# Invalid

    # To be added to hpq
    def __lt__(self, x):
        return False

turn_cost = 1000
# In hourly, just to have a pattern
# Added selfs to simply logic, no need to check
cost_pairs = {
        (Or.N, Or.N): 0,
        (Or.N, Or.E): 1*turn_cost,
        (Or.N, Or.S): 2*turn_cost,
        (Or.N, Or.W): 1*turn_cost,
        (Or.E, Or.E): 0,
        (Or.E, Or.N): 1*turn_cost,
        (Or.E, Or.W): 2*turn_cost,
        (Or.E, Or.S): 1*turn_cost,
        (Or.S, Or.S): 0,
        (Or.S, Or.E): 1*turn_cost,
        (Or.S, Or.N): 2*turn_cost,
        (Or.S, Or.W): 1*turn_cost,
        (Or.W, Or.W): 0,
        (Or.W, Or.N): 1*turn_cost,
        (Or.W, Or.E): 2*turn_cost,
        (Or.W, Or.S): 1*turn_cost,
        }

def print_mat(mat):
    [print(r) for r in mat]

def print_mat_pad(mat):
    [print([str(e).zfill(4) for e in r]) for r in mat]

def find_mat(mat, v):
    for ridx, row in enumerate(mat):
        for cidx, e in enumerate(row):
            if e == v:
                return (ridx, cidx)
    return None

def add_pos(p1, p2):
    return (p1[0]+p2[0], p1[1]+p2[1])

def get_mat(mat, p):
    return mat[p[0]][p[1]]

def set_mat(mat, p, v):
    mat[p[0]][p[1]] = v

def clone_mat(mat, v=-1):
    nm = []
    for r in mat:
        nm.append([v for _ in r])
    return nm

def possible_moves(mat, c, l, o):
    res = []
    for m, no in [((-1, 0), Or.N), ((1, 0), Or.S), ((0, -1), Or.W), ((0, 1), Or.E)]:
        # Next location
        nl = add_pos(l, m)
        if get_mat(mat, nl) != '#':
            nc = c + 1 + cost_pairs[o, no]
            res.append((nc, nl, no))
    return res

def apply_path_to_mat(mat, seen):
    nm = clone_mat(mat)
    for k, v in seen.items():
        r, c = k
        nm[r][c] = v
    return nm

def walk_path_dijstra(mat, start_loc_or, c=0):
    seen = dict()
    hpq = list([(c, start_loc_or[0], start_loc_or[1])])
    best = 0
    while len(hpq)>0:
        c, l, o = heapq.heappop(hpq)
        # We got to the end
        if get_mat(mat, l) == 'E':
            if best == 0 or best > c:
                best = c
        if (l, o) in seen and seen[(l, o)] < c:
            continue
        if best != 0 and c > best:
            continue
        seen[(l, o)] = c
        for nc, nl, no in possible_moves(mat, c, l, o):
            heapq.heappush(hpq, (nc, nl, no))
    return best, seen

def remove_orient_seen(seen):
    ns = dict()
    for k, v in seen.items():
        p, o = k
        if p in ns and ns[p] < v:
            continue
        ns[p] = v
    return ns

def get_path(mat, seen, vt=0):
    ns = remove_orient_seen(seen)
    path_taken = set()
    #print_mat_pad(apply_path_to_mat(mat, ns))
    dst_pos = find_mat(mat, 'E')
    cp = dst_pos
    cv = ns[dst_pos]
    #print(ns)
    while ns[cp] != vt:
        path_taken.add(cp)
        #print("GP", cp, cv)
        nlp = []
        nlv = 0
        for _, nl, _ in possible_moves(mat, 0, cp, Or.N):
            if nl not in ns:
                continue
            nlv = ns[nl]
            #print("GP NL", nl, nlv)
            if nlv == (cv-1) or nlv == (cv-1001):
                nlp.append(nl)
        # Should only be a single path
        if len(nlp) > 1:
            print(nlp)
            exit(2)
        cp = nlp[0]
        cv = ns[cp]
    path_taken.add(cp)
    return path_taken

def walk_all(mat, start_loc_or):
    cnt = 0
    seen = dict()
    good_paths = set()
    res = set()
    tgt, ps = walk_path_dijstra(mat, start_loc_or)
    print("Start walk all")
    good_paths |= get_path(mat, ps, 0)
    dq = deque([(0, start_loc_or[0], start_loc_or[1])])
    while len(dq) > 0:
        c, l, o = dq.popleft()
        # If checked
        if l in seen and seen[l] <= c:
            continue
        seen[l] = c
        cnt += 1
        if l not in good_paths:
            curpathbest, curpathseen = walk_path_dijstra(mat, (l, o), c)
            if curpathbest != tgt:
                continue
            print(l, o, c)
            good_paths |= get_path(mat, curpathseen, c)
        res.add(l)
        for nc, nl, no in possible_moves(mat, c, l, o):
            dq.append((nc, nl, no))
    print(cnt, len(res))
    return len(res)


def entry_func(inp: str):
    mat = [list(i) for i in inp.split('\n')]
    print_mat(mat)
    start_loc_or = (find_mat(mat, 'S'),Or.E)
    tot = walk_all(mat, start_loc_or)
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example_short(self):
        inp_str = """###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"""
        self.assertEqual(entry_func(inp_str), 45)

    def test_example(self):
        inp_str = """#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"""
        self.assertEqual(entry_func(inp_str), 64)
