import unittest, sys, heapq

def add_pos(p1, p2):
    return tuple([i + j for i, j in zip(p1, p2)])

def create_mat(dim, iv=0):
    return [[iv] * (dim[1]+1) for _ in range(dim[0]+1)]

def print_mat(mat):
    [print(r) for r in mat]

def print_mat_blocks(blocks, tgt):
    mat = create_mat(tgt, '.')
    for c, r in blocks:
        mat[r][c] = '#'
    print_mat(mat)

def walk_path_dijstra(blocks, tgt):
    start_loc = (0, 0)
    seen = set()
    hpq = list([(0, start_loc)])
    best = 0
    while len(hpq)>0:
        c, l = heapq.heappop(hpq)
        print(c, l)
        # We got to the end
        if l[0] == tgt[0] and l[1] == tgt[1]:
            if best == 0 or best > c:
                best = c
        if l in seen:
            continue
        if best != 0 and c > best:
            continue
        seen.add(l)
        print(c, l)
        for m in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            # Next location
            nl = add_pos(l, m)
            # Remove outside of our scope
            if nl[0] < 0 or nl[1] < 0 or nl[0] > tgt[0] or nl[1] > tgt[1]:
                continue
            # If not a barrier
            if nl not in blocks:
                nc = c + 1
                heapq.heappush(hpq, (nc, nl))
    return best

def entry_func(inp, tgt, amount):
    tot = 0
    blocks = set([tuple([int(j) for j in i.split(',')]) for i in inp.split('\n')[:amount]])
    print_mat_blocks(blocks, tgt)
    tot = walk_path_dijstra(blocks, tgt)
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1], (70,70), 1024))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"""
        self.assertEqual(entry_func(inp_str, (6,6), 12), 22)
