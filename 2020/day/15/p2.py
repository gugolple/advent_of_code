#!/usr/bin/env python3
import sys, itertools, unittest


def main(starting_nums: list[int], stop_at: int = 30000000) -> int:
    mem = {}
    for ix, num in enumerate(starting_nums):
        mem[num] = (-1, ix+1)  # (previous, latest)

    ix = len(starting_nums)
    latest = starting_nums[-1]
    while ix < stop_at:
        ix += 1
        if (latest not in mem) or (mem[latest][0] == -1):
            latest = 0
            mem[latest] = (mem[latest][1], ix)
        else:
            latest = mem[latest][1] - mem[latest][0]
            if latest not in mem:
                mem[latest] = (-1, ix)
            else:
                mem[latest] = (mem[latest][1], ix)
    return latest

# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):

    def test_basic_2020_itr(self):
        self.assertEqual(main([0,3,6]), 175594)

    def test_comp_1(self):
        self.assertEqual(main([1,3,2]), 2578)

    def test_comp_2(self):
        self.assertEqual(main([2,1,3]), 3544142)

    def test_comp_3(self):
        self.assertEqual(main([1,2,3]), 261214)

    def test_comp_4(self):
        self.assertEqual(main([2,3,1]), 6895259)

    def test_comp_5(self):
        self.assertEqual(main([3,2,1]), 18)

    def test_comp_6(self):
        self.assertEqual(main([3,1,2]), 362)


if __name__ == "__main__":
    rows = list(map(lambda x: int(x), sys.stdin.read().strip().split("\n")[0].split(',')))
    print(rows)

    print(main(rows))
