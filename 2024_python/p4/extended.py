import unittest, sys, heapq, re
from enum import Enum
import numpy as np

def entry_func(inp_str):
    tot = 0
    print(inp_str)
    print()
    inp_spl = inp_str.split('\n')
    pmatrix = [list(i) for i in inp_spl]
    nmatrix = np.array(pmatrix)

    print(nmatrix)
    valid_corners = ["MMSS", "MSSM", "SSMM", "SMMS"]
    for row_idx in range(1, nmatrix.shape[0]-1):
        for col_idx in range(1, len(nmatrix[0])-1):
            if nmatrix[row_idx,col_idx] == 'A':
                corners = ''.join([nmatrix[row_idx-1,col_idx-1],nmatrix[row_idx-1,col_idx+1],nmatrix[row_idx+1,col_idx+1],nmatrix[row_idx+1,col_idx-1]])
                if corners in valid_corners:
                    tot += 1
                print(row_idx, col_idx, corners)
    return tot

if __name__ == "__main__":
    np.set_printoptions(threshold=sys.maxsize)
    np.set_printoptions(linewidth=np.inf)
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):

    def test_example(self):
        print()
        inp_str = """.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
.........."""
        self.assertEqual(entry_func(inp_str), 9)
