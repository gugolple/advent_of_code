import unittest, sys

_SOLVE_DES_CACHE = dict()
def solve_des(avail_patt, des):
    if des == "":
        return 1
    if des in _SOLVE_DES_CACHE:
        return _SOLVE_DES_CACHE[des]
    res = 0
    for ap in avail_patt:
        nd = des.removeprefix(ap)
        #print(des, nd, ap)
        if len(des) - len(nd) == len(ap):
            res += solve_des(avail_patt, nd)
    _SOLVE_DES_CACHE[des] = res
    return res

                

def entry_func(inp: str):
    tot = 0
    avail_patt, desired = inp.split("\n\n")
    avail_patt = avail_patt.split(", ")
    desired = desired.split("\n")
    print(avail_patt)
    for des in desired:
        print(des)
        tot += solve_des(avail_patt, des)
        print(tot)
        #print(_SOLVE_DES_CACHE)
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"""
        self.assertEqual(entry_func(inp_str), 16)
