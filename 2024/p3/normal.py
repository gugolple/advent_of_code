import unittest, sys, heapq, re

def op(i: str):
    act_str = i[4:]
    act_str = act_str[:-1]
    l, r = act_str.split(",")
    return int(l) * int(r)

def entry_func(inp_str):
    tot = 0
    reg = re.compile(r"mul\(\d+,\d+\)")
    for l in inp_str.split('\n'):
        lr = re.findall(reg, l)
        for i in lr:
            tot += op(i)
    return tot



if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""
        self.assertEqual(entry_func(inp_str), 161)
