import unittest, sys

def find_last_occupied(list_blocks):
    for idx in reversed(range(len(list_blocks))):
        if list_blocks[idx][0] != -1:
            return idx

def find_first_free(list_blocks):
    for idx, i in enumerate(list_blocks):
        if i[0] == -1:
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
        if occupied:
            list_blocks.append((cid, i))
            cid += 1
        else:
            list_blocks.append((-1, i))
    list_blocks.append((-1, 0))
    print(list_blocks)
    ffidx = find_first_free(list_blocks)
    # Keep processing until free space is all at the end
    while ffidx < (len(list_blocks)-1):
    #for _ in range(2):
        idx_last_occupied = find_last_occupied(list_blocks)
        relocation = list_blocks[idx_last_occupied]
        #print()
        #print(list_blocks)
        #print(relocation)
        while relocation[1] > 0:
            idx_fist_free = find_first_free(list_blocks)
            if list_blocks[idx_fist_free][1] >= relocation[1]:
                del list_blocks[idx_last_occupied]
                list_blocks[idx_fist_free] = (list_blocks[idx_fist_free][0], list_blocks[idx_fist_free][1] - relocation[1])
                if list_blocks[idx_fist_free][1] == 0:
                    del list_blocks[idx_fist_free]
                list_blocks.insert(idx_fist_free, relocation)
                break
            else:
                # print(relocation)
                relocation = (relocation[0], relocation[1]-list_blocks[idx_fist_free][1])
                list_blocks[idx_fist_free] = (relocation[0], list_blocks[idx_fist_free][1]) 

        ffidx = find_first_free(list_blocks)
    print()
    print(list_blocks)
    pos = 0
    for tup in list_blocks:
        for c in range(tup[1]):
            if tup[0] >= 0:
                tr = pos * tup[0]
                print(pos, tup[0], tr)
                tot += tr
            pos += 1
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """2333133121414131402"""
        self.assertEqual(entry_func(inp_str), 1928)
