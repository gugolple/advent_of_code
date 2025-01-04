import unittest, sys

def entry_func(inp: str):
    tot = 0
    print(inp)
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """"""
        self.assertEqual(entry_func(inp_str), )
