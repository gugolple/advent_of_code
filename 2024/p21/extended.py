import unittest, sys
from enum import Enum
from itertools import pairwise, product

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
        return c, [['A']]
    if l in seen and seen[l] < c:
        return None
    seen[l] = c
    best = 0
    best_pathact = None
    for m, act in [((-1, 0), '^'), ((1, 0), 'v'), ((0, -1), '<'), ((0, 1), '>')]:
        nl = add_pos(l, m)
        if location_valid(matd, nl) and get_mat(mat, nl) != '#':
            nc = c+1
            res = recurse_path(mat, matd, nl, d, nc, act, seen)
            if res is not None:
                cst, pathact = res
                if best == 0 or best > cst:
                    best = cst
                    for pa in pathact:
                        pa.append(act)
                    best_pathact = pathact
                elif best == cst:
                    for pa in pathact:
                        pa.append(act)
                    best_pathact.extend(pathact)
    if best > 0:
        return best, best_pathact
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
                c, pathact = res
                tpa = ["".join(reversed(pa)) for pa in pathact]
                #print(c, path, pathact)
                orgdestcost[sddd] = tpa
    return orgdestcost

numpad_costs = calculate_costs(numpad)
#print(numpad_costs)
dirpad_costs = calculate_costs(dirpad)
#print(dirpad_costs)

def collapse_possibilities(ps):
    # This is absolutely dark magic, but tremendously useful
    # This operations return all the possibilities as strings
    return ["".join(x) for x in product(*ps)]

def dirpad_costs_solv(linp):
    #print("DP", linp)
    tb = "--"
    o = 'A'
    for sd in linp:
        #print("Path", sd)
        for d in list(sd):
            od = (o, d)
            dpc = dirpad_costs[od]
            #print(tb, "DP", o, d, "--", dpc)
            yield dpc
            o = d

def numpad_costs_solv(linp):
    #print("NP", linp)
    lp = ['A']
    lp.extend(linp)
    ps = []
    for o, d in pairwise(lp):
        od = (o, d)
        # This is only for numpad logic
        ps.append(numpad_costs[od])
    #print(ps)
    act_ans = collapse_possibilities(ps)
    #print(act_ans)
    return act_ans

def calculate_costs(ot, rl):
    return int(ot[:-1]) * len(rl)

_SOLV_CALCULATE_COSTS_CACHE = dict()
def solv_calculate_costs(linp, depth):
    ld = (tuple(linp), depth)
    if ld in _SOLV_CALCULATE_COSTS_CACHE:
        return _SOLV_CALCULATE_COSTS_CACHE[ld]
    #print(depth, linp)
    if depth == 0:
        return min([len(x) for x in linp])
    bstscr = float("inf")
    for pos in linp:
        curscr = 0
        #print("pos", pos)
        o = 'A'
        for d in pos:
            od = (o, d)
            nxtstp = dirpad_costs[od]
            curscr += solv_calculate_costs(nxtstp, depth-1)
            #print(od, nxtstp, curscr)
            o = d
        if curscr < bstscr:
            bstscr = curscr
    _SOLV_CALCULATE_COSTS_CACHE[ld] = bstscr
    return bstscr


def stringify(f):
    l = list()
    for r in f:
        l.append(r)
    return "".join(l)

def get_int_frm_cmd(cmd:str):
    return int(cmd[:-1])

def entry_func(inp: str, depth=25):
    tot = 0
    for cmd in inp.split("\n"):
        cb = float("inf")
        tot += get_int_frm_cmd(cmd)*solv_calculate_costs(numpad_costs_solv(cmd), depth)
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """029A
980A
179A
456A
379A"""
        self.assertEqual(entry_func(inp_str, 2), 126384)

    def test_example25(self):
        inp_str = """029A
980A
179A
456A
379A"""
        self.assertEqual(entry_func(inp_str, 25), 154115708116294)

    def test_d1(self):
        inp_str = """029A"""
        self.assertEqual(entry_func(inp_str, 0), 348)

    def test_d2(self):
        inp_str = """029A"""
        self.assertEqual(entry_func(inp_str, 1), 812)

    def test_d25(self):
        inp_str = """029A"""
        self.assertEqual(entry_func(inp_str, 25), 2379451789590)
