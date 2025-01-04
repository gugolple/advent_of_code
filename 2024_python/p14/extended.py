import unittest, sys

def calc_pos(dim, p, v, itr):
    return ((p[0] + v[0]*itr)%dim[0], (p[1] + v[1]*itr)%dim[1])

def cuad_pert(dim, p):
    midh = dim[0]//2
    midv = dim[1]//2
    ph, pv = p
    if ph == midh or pv == midv:
        return 0
    if ph < midh and pv < midv:
        return 1
    if ph > midh and pv < midv:
        return 2
    if ph < midh and pv > midv:
        return 3
    if ph > midh and pv > midv:
        return 4

def print_mat(mat):
    for row in mat:
        print(row)

def create_mat(dim, sp: set):
    mat = list()
    for _ in range(dim[0]):
        mat.append([0] * dim[1])
    for h, v in sp:
        mat[h][v] = 1
    return mat


def entry_func(inp: str, dim = (101, 103)):
    print(inp)
    start_data = list()
    for r in inp.split("\n"):
        p, v = [i[2:] for i in r.split(" ")]
        p = [int(i) for i in p.split(",")]
        v = [int(i) for i in v.split(",")]
        start_data.append((p, v))
    itr = 0
    found = False
    while not found:
        points = set([calc_pos(dim, p, v, itr) for p, v in start_data])
        print(itr)
        print_mat(create_mat(dim, points))
        if len(points) == 500:
            break
        itr += 1
    return itr 

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))
