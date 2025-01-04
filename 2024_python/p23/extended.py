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

def generate_recursv(cons, lc, depth):
    if depth == 1:
        print("--RE", lc)
        return set([tuple(sorted(lc))])
    set_pos = set()
    set_pos |= cons[lc[0]]
    for le in lc[1:]:
        cp = cons[le]
        if cp is not None:
            set_pos &= cp
    print("lc", lc, set_pos)
    resl = 0
    res = set()
    for rp in set_pos:
        nlc = deepcopy(lc)
        nlc.append(rp)
        cp = generate_recursv(cons, nlc, depth-1)
        print("ICP", cp)
        for ccp in cp:
            cpl = len(ccp)
            if cpl>resl:
                resl = cpl
                res = set([ccp])
            elif cpl == resl:
                res.add(ccp)
    return res

def generate_all_recursv(cons, depth):
    print(cons)
    total = set()
    for start in sorted(cons.keys()):
        #print(start)
        total |= generate_recursv(cons, [start], depth)
    return total

def generate_max_recursv(cons, lc):
    set_pos = set()
    set_pos |= cons[lc[0]]
    for le in lc[1:]:
        cp = cons[le]
        if cp is not None:
            set_pos &= cp
    #print("lc", lc, set_pos)
    resl = 0
    res = set()
    if len(set_pos) > 0:
        for rp in set_pos:
            nlc = deepcopy(lc)
            nlc.append(rp)
            cp = generate_max_recursv(cons, nlc)
            #print("ICP", cp)
            for ccp in cp:
                cpl = len(ccp)
                if cpl>resl:
                    resl = cpl
                    res = set([ccp])
                elif cpl == resl:
                    res.add(ccp)
    else:
        res = set([tuple(sorted(lc))])
        #print("Limit!", res)
    return res

def generate_all_max_recursv(cons):
    #print(cons)
    res = None
    best = 0
    #print("Start search")
    for start in sorted(cons.keys()):
        #print(start)
        c = generate_max_recursv(cons, [start])
        for ce in c:
            print(ce)
            lce = len(ce)
            if lce > best:
                best = lce
                res = set([ce])
            elif lce == best:
                res.add(ce)
    return res


def generate_triplets(cons):
    trip = set()
    for start in cons:
        for second in cons[start]:
            if start == second:
                continue
            for third in cons[second]:
                if start == third or second == third:
                    continue
                if third not in cons[start]:
                    continue
                #print("TRIPL", start, second, third)
                tpl = tuple(sorted([start, second, third]))
                if tpl not in trip:
                    trip.add(tpl)
    return trip

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

    def test_triplets(self):
        self.assertEqual(len(generate_triplets(self.pairs)), 12)

    def test_triplets_rec(self):
        ans = generate_all_recursv(self.pairs, 3)
        print(ans)
        self.assertEqual(len(ans), 12)
