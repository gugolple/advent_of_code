import unittest, sys

def find_id(list_blocks, bid):
    for idx in reversed(range(len(list_blocks))):
        if list_blocks[idx][0] == bid:
            return idx

def list_all_free(list_blocks):
    for idx, i in enumerate(list_blocks):
        if i[0] == -1:
            yield idx

def entry_func(inp: str):
    tot = 0
    print(inp)
    occupied = False
    cid = 0
    list_blocks = []
    for i in [int(i) for i in inp]:
        occupied = not occupied
        if i == 0: continue
        if occupied:
            list_blocks.append((cid, i))
            cid += 1
        else:
            list_blocks.append((-1, i))
    list_blocks.append((-1, 0))
    print(list_blocks)
    for i in reversed(range(1,cid)):
        ridx = find_id(list_blocks,i)
        print(i, ridx)
        relocation = list_blocks[ridx]
        for fidx in list_all_free(list_blocks):
            if fidx > ridx:
                break
            if list_blocks[fidx][1] > relocation[1]:
                tsize = list_blocks[fidx][1] - relocation[1]
                #print("GT", tsize)
                list_blocks[fidx] = list_blocks[ridx]
                list_blocks[ridx] = (-1, list_blocks[fidx][1])
                list_blocks.insert(fidx+1, (-1, tsize))
                break
            if list_blocks[fidx][1] == relocation[1]:
                #print("EQ")
                list_blocks[fidx] = list_blocks[ridx]
                list_blocks[ridx] = (-1, list_blocks[fidx][1])
                break
        #print(list_blocks)
        #input()
    print(list_blocks)
    pos = 0
    for tup in list_blocks:
        for c in range(tup[1]):
            if tup[0] >= 0:
                tr = pos * tup[0]
                tot += tr
            pos += 1
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """2333133121414131402"""
        self.assertEqual(entry_func(inp_str), 2858)
