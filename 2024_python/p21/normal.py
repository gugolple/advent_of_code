import unittest, sys
from enum import Enum
from itertools import pairwise

INV = '#'

class EDir(Enum):
    U = '^'
    R = '>'
    D = 'v'
    L = '<'
    A = 'A'
    I = '#'

numpad = [
        list("789"),
        list("456"),
        list("123"),
        list("#0A"),
    ]

dirpad = [
        list("#^A"),
        list("<v>"),
    ]

def create_mat(dim, iv=0):
    return [[iv] * (dim[1]+1) for _ in range(dim[0]+1)]

def print_mat(mat):
    [print(r) for r in mat]

def add_pos(p1, p2):
    return tuple([i + j for i, j in zip(p1, p2)])

def get_mat(mat, l):
    return mat[l[0]][l[1]]

def set_mat(mat, l, v):
    mat[l[0]][l[1]] = v

def location_invalid(matd, l):
    mr, mc = matd
    return (l[0] < 0 or l[1] < 0 or l[0] >= mr or l[1] >= mc)

def location_valid(matd, l):
    return not location_invalid(matd, l)

def recurse_path(mat, matd, l, d, c=0, ld='', seen=None):
    #print(l)
    if seen is None:
        seen = dict()
    if l == d:
        # Add an A pulse to the end
        return c, [l], ['A']
    if l in seen and seen[l] < c:
        return None
    seen[l] = c
    best = 0
    best_path = None
    best_pathact = None
    for m, act in [((-1, 0), '^'), ((1, 0), 'v'), ((0, -1), '<'), ((0, 1), '>')]:
        nl = add_pos(l, m)
        if location_valid(matd, nl) and get_mat(mat, nl) != '#':
            nc = c+1
            if ld != act:
                nc += 100
            res = recurse_path(mat, matd, nl, d, nc, act, seen)
            if res is not None:
                cst, path, pathact = res
                if best == 0 or cst < best:
                    best = cst
                    path.append(l)
                    pathact.append(act)
                    best_path = path
                    best_pathact = pathact
    if best > 0:
        return best, best_path, best_pathact
    return None

def calculate_costs(mat):
    matd = (len(mat), len(mat[0]))
    orgdestcost = dict()
    tpos_loc = [[(ridx, cidx) for cidx,_ in enumerate(row)] for ridx, row in enumerate(mat)]
    pos_loc = []
    for pl in tpos_loc:
        pos_loc.extend(pl)
    for s in pos_loc:
        if get_mat(mat, s) == '#':
            continue
        sd = get_mat(mat, s)
        for d in pos_loc:
            if get_mat(mat, d) == '#':
                continue
            dd = get_mat(mat, d)
            # Start destination tuple to locate any pair of digits
            sddd = (sd, dd)
            if s == d:
                orgdestcost[sddd] = "A"
            #print("Test path", s, sd, d, dd)
            res = recurse_path(mat, matd, s, d)
            if res is not None:
                c, path, pathact = res
                path = "".join([get_mat(mat, i) for i in reversed(path)])
                pathact = "".join(reversed(pathact))
                #print(c, path, pathact)
                orgdestcost[sddd] = pathact
    return orgdestcost

numpad_costs = calculate_costs(numpad)
print(numpad_costs)
dirpad_costs = calculate_costs(dirpad)
print(dirpad_costs)

def numpad_costs_solv(linp):
    lp = ['A']
    lp.extend(linp)
    for o, d in pairwise(lp):
        od = (o, d)
        npc = numpad_costs[od]
        print("NP", o, d, "--", npc)
        yield npc

def dirpad_costs_solv(linp, f=False):
    tb = ""
    if f:
        tb = "----"
    o = 'A'
    for sd in linp:
        #print("Path", sd)
        for d in list(sd):
            od = (o, d)
            dpc = dirpad_costs[od]
            print(tb, "DP", o, d, "--", dpc)
            yield dpc
            o = d

def entry_func(inp: str):
    tot = 0
    for cmd in inp.split("\n"):
        res = "".join(dirpad_costs_solv(dirpad_costs_solv(numpad_costs_solv(cmd)), f=True))
        cres = len(res)
        print(cmd, cres, res)
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example_ncs(self):
        inp_str = """029A"""
        self.assertEqual("".join(numpad_costs_solv(list(inp_str))), "<A^A^^>AvvvA")

    def test_example_dps(self):
        inp_str = """029A"""
        self.assertEqual("".join(dirpad_costs_solv(numpad_costs_solv(inp_str))), "v<<A>^>A<A>A<AAv>A^Av<AAA^>A")

    def test_example_ddps(self):
        inp_str = """029A: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
980A: <v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A
179A: <v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
456A: <v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A
379A: <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"""
        for l in inp_str.split("\n"):
            iv, rv = l.split(": ")
            actres = "".join(dirpad_costs_solv(dirpad_costs_solv(numpad_costs_solv(iv)), f=True))
            print(iv)
            print(rv)
            print(actres)
            self.assertEqual(len(actres), len(rv))

    def test_example(self):
        inp_str = """029A
980A
179A
456A
379A"""
        self.assertEqual(entry_func(inp_str), 126384)
