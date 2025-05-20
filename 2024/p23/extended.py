import unittest, sys
from copy import deepcopy

def generate_connections(inp):
    con = dict()
    con_id_cnt = 0
    con_id = dict()
    for p in inp.split("\n"):
        l, r = p.split("-")
        if l in con:
            con[l].add(r)
        else:
            con[l] = set([r])
        if r in con:
            con[r].add(l)
        else:
            con[r] = set([l])
    return con

def generate_single(cons, lc):
    pos_con = set()
    pos_con |= cons[lc[0]]
    for e in lc[1:]:
        pos_con &= cons[e]
    res = set()
    for pc in pos_con:
        res.add(tuple(sorted([*lc, pc])))
    #print(res)
    return res
        

def generate_all_max_recursv(cons):
    itr = 0
    set_starts = set([tuple([i]) for i in cons.keys()])
    print(set_starts)
    nxt_itr = deepcopy(set_starts)
    while len(nxt_itr) > 0:
        itr += 1
        cur_itr = set()
        for start in nxt_itr:
            ns = generate_single(cons, start)
            #print("start", start, ns)
            for ps in ns:
                cur_itr.add(ps)
        set_starts = nxt_itr
        nxt_itr = cur_itr
        print(itr)
    return set_starts

def entry_func(inp: str):
    tot = 0
    #print(inp)
    pairs = generate_connections(inp)
    ans = generate_all_max_recursv(pairs)
    print(ans)
    act_ans = ",".join(ans.pop())
    return act_ans

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    inp_str = """kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn"""
    pairs = generate_connections(inp_str)

    def test_example(self):
        ans = generate_all_max_recursv(self.pairs)
        act_ans = ",".join(ans.pop())
        self.assertEqual(act_ans, "co,de,ka,ta")
