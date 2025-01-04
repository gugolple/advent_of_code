import unittest, sys, heapq, re
import numpy as np

reg = re.compile(r"(XMAS|SAMX)")
def count_matches(l: str):
    tot = 0
    cm = reg.search(l)
    while cm:
        tot += 1
        print(cm, cm.start(), cm.end())
        cm = reg.search(l, cm.start()+1)
    return tot

def create_horizontals(nmatrix):
    print(nmatrix)
    for off in range(nmatrix.shape[0]):
        yield ''.join(nmatrix[off, :])

def create_diag(nmatrix):
    print(nmatrix)
    for off in range(-nmatrix.shape[0], nmatrix.shape[1]):
        yield ''.join(np.diagonal(nmatrix,off))


def entry_func(inp_str):
    print(inp_str)
    print()
    inp_spl = inp_str.split('\n')
    pmatrix = [list(i) for i in inp_spl]
    nmatrix = np.array(pmatrix)
    tnmatrix = np.rot90(nmatrix)
    tot = 0
    # Horizontal
    for l in create_horizontals(nmatrix):
        print(l)
        tot += count_matches(l)
    print(tot)
    print()

    # Vertical
    for l in create_horizontals(tnmatrix):
        print(l)
        tot += count_matches(l)
    print(tot)
    print()

    # Diagonals
    for l in create_diag(nmatrix):
        print(l)
        tot += count_matches(l)
    print(tot)
    print()

    for l in create_diag(tnmatrix):
        print(l)
        tot += count_matches(l)
    print(tot)
    print()

    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_a_pers(self):
        inp_str = """MMMSXXMASM
MSAMXMSMSA
MXMXAXMASX"""
        self.assertEqual(entry_func(inp_str), 3)

    def test_example_small(self):
        print()
        inp_str = """..X...
.SAMX.
.A..A.
XMAS.S
.X...."""
        self.assertEqual(entry_func(inp_str), 4)

    def test_example(self):
        print()
        inp_str = """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""
        self.assertEqual(entry_func(inp_str), 18)
