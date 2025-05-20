import unittest, sys, heapq

def reduce_list(lt: list[int]):
    res_dict = dict()
    for i in lt:
        if i in res_dict:
            res_dict[i]+=1
        else:
            res_dict[i]=1
    return res_dict


def entry_func(inp_str):
    left = list()
    right = list()
    for l in inp_str.split('\n'):
        if l == "":
            break
        l, r = l.split("   ")
        left.append(int(l))
        right.append(int(r))

    reduced_left = reduce_list(left)
    reduced_right = reduce_list(right)

    tot = 0
    for key in reduced_left.keys():
        if key in reduced_right:
            tot += reduced_left[key] * reduced_right[key] * key

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
        self.assertEqual(entry_func(inp_str), 31)
