import unittest, sys

def operation(stone):
    ls = len(stone)
    if stone == '0':
        return ['1']
    elif ls%2 == 0:
        rs = stone[ls//2:].lstrip('0')
        if rs == '':
            rs = '0'
        return [stone[:ls//2], rs]
    else:
        return [str(int(stone)*2024)]


def recursion(inp, seen, lim, depth=0):
    tot = 0
    # Seen
    lt = (inp, depth)
    if lt in seen:
        tot = seen[lt]
        print("hit", tot)
        return tot
    # Logic
    rs = operation(inp)
    print(inp, rs, depth)
    if depth != lim:
        for n in rs:
            tot += recursion(n, seen, lim, depth+1)
    # Target
    else:
        tot = len(rs)
    print(inp, rs, depth, "-", tot)
    seen[lt] = tot
    return tot


def entry_func(inp: str, iters=75):
    tot = 0
    stones = [i for i in inp.split(" ")]
    seen = dict()
    print()
    print()
    print()
    print()
    for stone in stones:
        print("----------Start rec", stone, tot)
        tot += recursion(stone, seen, iters-1)
    print(seen)
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example_other(self):
        inp_str = """0 1 10 99 999"""
        self.assertEqual(entry_func(inp_str, 1), 7)

    def test_example_short_1(self):
        inp_str = """125 17"""
        self.assertEqual(entry_func(inp_str, 1), 3)

    def test_example_short_2(self):
        inp_str = """125 17"""
        self.assertEqual(entry_func(inp_str, 2), 4)

    def test_example_short_3(self):
        inp_str = """125 17"""
        self.assertEqual(entry_func(inp_str, 3), 5)

    def test_example_short_4(self):
        inp_str = """125 17"""
        self.assertEqual(entry_func(inp_str, 4), 9)

    def test_example_short_5(self):
        inp_str = """125 17"""
        self.assertEqual(entry_func(inp_str, 5), 13)

    def test_example_short_6(self):
        inp_str = """125 17"""
        self.assertEqual(entry_func(inp_str, 6), 22)

    def test_example(self):
        inp_str = """125 17"""
        self.assertEqual(entry_func(inp_str, 25), 55312)
