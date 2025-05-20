import unittest, sys, heapq

def entry_func(inp_str):
    left = list()
    right = list()
    for l in inp_str.split('\n'):
        if l == "":
            break
        l, r = l.split("   ")
        left.append(int(l))
        right.append(int(r))

    ql = heapq.heapify(left)
    qr = heapq.heapify(right)

    tot = 0
    while left:
        l = heapq.heappop(left)
        r = heapq.heappop(right)
        tot += abs(l - r)

    return tot





if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """3   4
4   3
2   5
1   3
3   9
3   3"""
        self.assertEqual(entry_func(inp_str), 11)
