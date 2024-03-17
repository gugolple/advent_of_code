#!/usr/bin/env python3
import sys, itertools, unittest


def main(rows, iters=2020):
    mem = dict()
    # Basic answer, just the numbers
    if iters <= len(rows):
        return rows[iters-1]

    # Initialize
    for i, cn in enumerate(rows):
        mem[cn] = (-1, i+1)
    last = rows[-1]

    # Logic
    for i in range(len(rows)+1, iters+1):
        if(last not in mem):
            last = 0
            mem[last] = (-1, i)
        elif(mem[last][0] == -1):
            last = 0
            if last not in mem:
                mem[last] = (-1, i)
            else:
                mem[last] = (mem[last][1], i)
        else:
            last = mem[last][1] - mem[last][0]
            if last not in mem:
                mem[last] = (-1, i)
            else:
                mem[last] = (mem[last][1], i)
    return last


# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):
    basic_rows = [0, 3, 6]
    def test_basic_04_itr(self):
        self.assertEqual(main(self.basic_rows, 4), 0)

    def test_basic_05_itr(self):
        self.assertEqual(main(self.basic_rows, 5), 3)

    def test_basic_06_itr(self):
        self.assertEqual(main(self.basic_rows, 6), 3)

    def test_basic_07_itr(self):
        self.assertEqual(main(self.basic_rows, 7), 1)

    def test_basic_08_itr(self):
        self.assertEqual(main(self.basic_rows, 8), 0)

    def test_basic_09_itr(self):
        self.assertEqual(main(self.basic_rows, 9), 4)

    def test_basic_10_itr(self):
        self.assertEqual(main(self.basic_rows, 10), 0)

    def test_basic_2020_itr(self):
        self.assertEqual(main(self.basic_rows, 2020), 436)

    def test_comp_1(self):
        self.assertEqual(main([1,3,2]), 1)

    def test_comp_2(self):
        self.assertEqual(main([2,1,3]), 10)

    def test_comp_3(self):
        self.assertEqual(main([1,2,3]), 27)

    def test_comp_4(self):
        self.assertEqual(main([2,3,1]), 78)

    def test_comp_5(self):
        self.assertEqual(main([3,2,1]), 438)

    def test_comp_6(self):
        self.assertEqual(main([3,1,2]), 1836)


if __name__ == "__main__":
    rows = sys.stdin.read().strip().split("\n")[0].split(',')

    print(main(rows, 2022))
