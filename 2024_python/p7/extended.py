import unittest, sys

def recursive(tgt, l, val=0):
    if len(l) == 0:
        if tgt == val:
            return True
        else:
            return False
    v = int(l[0])
    if recursive(tgt, l[1:], val+v):
        return True
    if recursive(tgt, l[1:], val*v):
        return True
    dig = len(l[0])
    if recursive(tgt, l[1:], val*(10**dig)+v):
        return True
    return False


def entry_func(inp: str):
    tot = 0
    lines = inp.split("\n")
    print("\n".join(lines))
    for line in lines:
        tgt, src = line.split(": ")
        if recursive(int(tgt), src.split(" ")):
            tot += int(tgt)
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""
        self.assertEqual(entry_func(inp_str), 11387)
