import unittest, sys, heapq
from collections import deque
from copy import deepcopy

def search_mat(mat, v):
    for ridx, row in enumerate(mat):
        for cidx, e in enumerate(row):
            if e == v:
                return ridx, cidx

def get_mat(mat, l):
    return mat[l[0]][l[1]]

def set_mat(mat, l, v):
    mat[l[0]][l[1]] = v

def add_pos(p1, p2):
    return tuple([i + j for i, j in zip(p1, p2)])

def create_mat(dim, iv=0):
    return [[iv] * (dim[1]+1) for _ in range(dim[0]+1)]

def print_mat(mat):
    [print(r) for r in mat]

def location_invalid(matd, l):
    mr, mc = matd
    return (l[0] < 0 or l[1] < 0 or l[0] >= mr or l[1] >= mc)

def walk_path_dijstra(mat):
    mr = len(mat)
    mc = len(mat[0])
    matd = (mr, mc)
    start_loc = search_mat(mat, 'S')
    seen = dict()
    hpq = list([(0, start_loc)])
    best = 0
    while len(hpq)>0:
        c, l = heapq.heappop(hpq)
        #print(c, l)
        # We got to the end
        if get_mat(mat, l) == 'E':
            if best == 0 or best > c:
                #print("Bested?", c)
                best = c
                seen[l] = c
        if l in seen and seen[l] < c:
            continue
        # If known ba
        seen[l] = c
        if best != 0 and c > best:
            continue
        #print(c, l)
        for m in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            # Next location
            nl = add_pos(l, m)
            # Remove outside of our scope
            if location_invalid(matd, nl):
                continue
            # If not a barrier
            if get_mat(mat, nl) != '#':
                nc = c + 1
                heapq.heappush(hpq, (nc, nl))
    return best, seen

def get_best_path(matd, seen, find_pos, l, pth=None):
    if pth is None:
        pth = deque()
    if l == find_pos:
        return pth
    #print(l)
    for m in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        # Next location
        nl = add_pos(l, m)
        # Remove outside of our scope
        if location_invalid(matd, nl):
            continue
        if nl in seen and (seen[nl]+1) == seen[l]:
            pth.append(nl)
            res = get_best_path(matd, seen, find_pos, nl, pth)
            if res is not None:
                return pth
            pth.pop()
    return None

def print_mat_set(omat, s, sv):
    mat = deepcopy(omat)
    for r, c in s:
        if mat[r][c] == '.':
            mat[r][c] = sv
    print_mat(mat)

def generate_area_inf(s:int):
    l = []
    for ridx in range(-s, s+1):
        cs = s - abs(ridx)
        for cidx in range(-cs, cs+1):
            l.append((ridx, cidx))
    return l

def dist_pos(p1, p2):
    return sum([abs(e1 - e2) for e1, e2 in zip(p1, p2)])

# Give an iterator with all the possible skips
def skip_pos(mat, matd, area_inf, l):
    for m in area_inf:
        nl = add_pos(l, m)
        if not location_invalid(matd, nl) and get_mat(mat, nl) != '#':
            #print(l, m)
            yield nl

def entry_func(inp: str):
    area_inf = generate_area_inf(20)
    tot = 0
    mat = [list(i) for i in inp.split("\n")]
    print_mat(mat)
    print()
    mr = len(mat)
    mc = len(mat[0])
    matd = (mr, mc)
    score, seen = walk_path_dijstra(mat)
    path = list(get_best_path(matd, seen, search_mat(mat, 'S'), search_mat(mat, 'E')))
    print(path)
    print_mat_set(mat, path, 'O')
    possible_shorts = dict()
    for l in path:
        for nl in skip_pos(mat, matd, area_inf, l):
            #print("possible short", l, pw, pp)
            # Known seen
            if nl in seen:
                # If the next location is closer to the solution
                if seen[nl] > seen[l]:
                    cs = seen[nl] - seen[l] - dist_pos(l, nl)
                    lnl = (l, nl)
                    if lnl not in possible_shorts or possible_shorts[lnl] < cs:
                        possible_shorts[lnl] = cs
            else:
                print("Not known! D:")
    size_to_shortcut = dict()
    for k, v in possible_shorts.items():
        #print("PS", k, v)
        if v in size_to_shortcut:
            size_to_shortcut[v].append(k)
        else:
            size_to_shortcut[v] = [k]
    ext_res = []
    for k in sorted(size_to_shortcut.keys()):
        lps = len(size_to_shortcut[k])
        ext_res.append((k, lps))
        #print(k, lps, size_to_shortcut[k])
        if k >= 100:
            tot += lps
    return tot, ext_res

if __name__ == "__main__":
    sys.setrecursionlimit(150000)
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1])[0])

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"""
        tot, ext_res = entry_func(inp_str)
        all_ans = [
                (50, 32),
                (52, 31),
                (54, 29),
                (56, 39),
                (58, 25),
                (60, 23),
                (62, 20),
                (64, 19),
                (66, 12),
                (68, 14),
                (70, 12),
                (72, 22),
                (74, 4),
                (76, 3),
            ]
        idx_50 = 0
        for idx, v in enumerate(ext_res):
            sc, items = v
            #print("TDBG", sc, items)
            if sc == 50:
                idx_50 = idx
                break
        for ga, ea in zip(ext_res[idx_50:], all_ans):
            print(ga, ea)
            self.assertEqual(ga[0], ea[0])
            self.assertEqual(ga[1], ea[1])
