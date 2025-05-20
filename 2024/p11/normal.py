import unittest, sys

def operation(stone):
    ls = len(stone)
    if stone == '0':
        return '1'
    elif ls%2 == 0:
        rs = stone[ls//2:].lstrip('0')
        if rs == '':
            rs = '0'
        return stone[:ls//2], rs
    else:
        return str(int(stone)*2024)


def entry_func(inp: str, iters=25):
    tot = 0
    #print()
    #print()
    #print()
    stones = [i for i in inp.split(" ")]
    for _ in range(iters):
        #print(stones)
        ns = list()
        for idx, s in enumerate(stones):
            rs = operation(s)
            #print(idx, s, rs)
            if isinstance(rs, tuple):
                ns.extend(rs)
            else:
                ns.append(rs)
        stones = ns
    #print(stones)
    return len(stones)

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """125 17"""
        self.assertEqual(entry_func(inp_str), 55312)

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

    def test_example_other(self):
        inp_str = """0 1 10 99 999"""
        self.assertEqual(entry_func(inp_str, 1), 7)
