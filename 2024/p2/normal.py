import unittest, sys, heapq

def check_dif(dif: int):
    return dif >=1 and dif <=3

def check_dec(li: list[int]):
    last = li[0]
    for i in li[1:]:
        dif = last - i
        if not check_dif(dif):
            break
        last = i
    else:
        return True
    return False

def check_incr(li: list[int]):
    last = li[0]
    for i in li[1:]:
        dif = i - last
        if not check_dif(dif):
            break
        last = i
    else:
        return True
    return False

def check_valid(li: list[int]):
    if li[1] > li[0]:
        return check_incr(li)
    return check_dec(li)



def entry_func(inp_str):
    left = list()
    right = list()
    tot = 0
    for l in inp_str.split('\n'):
        if l == "":
            break
        li = [int(i) for i in l.split(" ")]
        if check_valid(li):
            tot += 1
    return tot



if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""
        self.assertEqual(entry_func(inp_str), 2)
