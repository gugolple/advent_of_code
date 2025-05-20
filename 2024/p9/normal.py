import unittest, sys

def find_last_occupied(list_blocks):
    for idx in reversed(range(len(list_blocks))):
        if list_blocks[idx] != -1:
            return idx

def find_first_free(list_blocks):
    for idx, i in enumerate(list_blocks):
        if i == -1:
            return idx

def entry_func(inp: str):
    tot = 0
    print(inp)
    occupied = False
    cid = 0
    list_blocks = []
    for i in [int(i) for i in inp]:
        occupied = not occupied
        if i == 0: continue
        nv = cid
        if occupied:
            cid += 1
        else:
            nv = -1
        for _ in range(i):
            list_blocks.append(nv)
    print(list_blocks)
    ff = find_first_free(list_blocks)
    lo = find_last_occupied(list_blocks)
    while ff < lo:
        list_blocks[ff] = list_blocks[lo]
        list_blocks[lo] = -1
        ff = find_first_free(list_blocks)
        lo = find_last_occupied(list_blocks)
    print(list_blocks)
    for pos, v in enumerate(list_blocks):
        if v == -1: continue
        tot += pos * v
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """2333133121414131402"""
        self.assertEqual(entry_func(inp_str), 1928)
