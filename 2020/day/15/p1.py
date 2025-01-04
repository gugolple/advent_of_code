#!/usr/bin/env python3
import sys, itertools, unittest


def main(rows, iters=2020):
    most_recent = dict()
    secm_recent = dict()
    # Basic answer, just the numbers
    if iters <= len(rows):
        return rows[iters-1]

    # Initialize
    for i, cn in enumerate(rows):
        most_recent[cn] = i
    last_spoken = rows[-1]

    # Logic
    for i in range(len(rows), iters):
        # Reached target, can remove?
        if i == iters:
            break
        # Initial value
        nn = None
        # If seen previously
        if last_spoken in secm_recent:
            nn = i -most_recent[last_spoken]
            secm_recent[last_spoken] = most_recent[last_spoken]
        # If just seen once
        elif last_spoken in most_recent:
            secm_recent[last_spoken] = most_recent[last_spoken]
            nn = i -most_recent[last_spoken] -1
        else:
            # New number
            nn = 0
        most_recent[last_spoken] = i
        #print(i, "-->",  nn)
        last_spoken = nn
    return last_spoken


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
    for r in rows:
        print(r)
    print()

    main(rows)
