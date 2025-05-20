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

def iter_equals(l1, l2):
    for l, r in zip(l1, l2):
        if l!=r:
            return False
    return True

def check_square(sp: set):
    for p in sp:
        l = 0
        np = (p[0], p[1]+1)
        next_itr = False
        # Check to the right
        while np in sp:
            print("-", np)
            next_itr = True
            l += 1
            np = (np[0], np[1]+1)
        if next_itr:
            next_itr = False
            # Check down
            np = (np[0]+1, np[1]-1)
            while np in sp:
                print("-", np)
                next_itr = True
                l += 1
                np = (np[0]+1, np[1])
            if next_itr:
                next_itr = False
                # Check left
                np = (np[0]-1, np[1]-1)
                while np in sp:
                    print("-", np)
                    next_itr = True
                    l += 1
                    np = (np[0], np[1]-1)
                if next_itr:
                    next_itr = False
                    # Check up
                    np = (np[0]-1, np[1]+1)
                    while np in sp:
                        print("-", np)
                        next_itr = True
                        l += 1
                        np = (np[0]-1, np[1])
                    if next_itr:
                        np = (np[0]+1, np[1])
                        if iter_equals(p, np) and l > 16:
                            return True
    return False



def entry_func(inp: str, dim = (101, 103)):
    print(inp)
    start_data = list()
    for r in inp.split("\n"):
        p, v = [i[2:] for i in r.split(" ")]
        p = [int(i) for i in p.split(",")]
        v = [int(i) for i in v.split(",")]
        start_data.append((p, v))
    itr = 0
    while True:
        points = set([calc_pos(dim, p, v, itr) for p, v in start_data])
        print(itr)
        if check_square(points):
            print("CSE")
            break
        itr += 1
    #print_mat(create_mat(dim, points))
    return itr 

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))
