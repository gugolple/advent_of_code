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


def entry_func(inp: str, dim = (101, 103), itr=100):
    print(inp)
    ltot = [0] * 5
    for r in inp.split("\n"):
        p, v = [i[2:] for i in r.split(" ")]
        p = [int(i) for i in p.split(",")]
        v = [int(i) for i in v.split(",")]
        print(p, v)
        print()
        fp = calc_pos(dim, p, v, itr)
        cp = cuad_pert(dim, fp)
        print(fp, cp)
        ltot[cp] += 1
    print(ltot)
    tot = 1
    for lt in ltot[1:]:
        tot *= lt
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"""
        self.assertEqual(entry_func(inp_str, (11, 7)), 12)
