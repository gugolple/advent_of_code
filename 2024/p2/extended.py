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
    print(li)
    if li[1] > li[0]:
        return check_incr(li)
    return check_dec(li)



def entry_func(inp_str):
    left = list()
    right = list()
    tot = 0
    for l in inp_str.split('\n'):
        acc = False
        if l == "":
            break
        li = [int(i) for i in l.split(" ")]
        if check_valid(li):
            tot += 1
            acc = True
        else:
            for i in range(len(li)-1):
                auxli = li[0:i] + li[i+1:]
                if check_valid(auxli):
                    acc = True
                    tot += 1
                    break
            else:
                auxli = li[:-1]
                if check_valid(auxli):
                    acc = True
                    tot += 1
        print(acc, li)
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
        self.assertEqual(entry_func(inp_str), 4)

    def test_pers(self):
        inp_str = """38 41 44 47 50 47"""
        self.assertEqual(entry_func(inp_str), 1)
