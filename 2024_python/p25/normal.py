import unittest, sys

def printl(l):
    [print(r) for r in l]

def convert_blk_to_digits(blk: str):
    lk = "k"
    char = "#"
    if all([i == "#" for i in blk[0]]):
        lk = "l"
    vals = [0] * len(blk[0])
    for row in blk[1:-1]:
        for cidx, e in enumerate(row):
            vals[cidx] += e == char
    vals.append(lk)
    #printl(blk)
    #print(vals)
    return tuple(vals)

def inverse_lock(lck):
    return tuple([5-v for v in lck])

def proces_inp(inp: str):
    res = {"k": set(), "l": set(), "il": set()}
    for blk in inp.split("\n\n"):
        t = convert_blk_to_digits(blk.split("\n"))
        res[t[-1]].add(tuple(t[:-1]))
        if t[-1] == "l":
            res["il"].add(inverse_lock(t[:-1]))
    return res

def addl(l1, l2):
    return [e1+e2 for e1, e2 in zip(l1, l2)]

def entry_func(inp: str):
    tot = 0
    res = proces_inp(inp)
    for l in res["l"]:
        for k in res["k"]:
            if max(addl(l, k)) <= 5:
                tot += 1
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"""
        self.assertEqual(entry_func(inp_str), 3)
