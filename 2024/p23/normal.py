import unittest, sys

def generate_connections(inp):
    con = dict()
    con_id_cnt = 0
    con_id = dict()
    for p in inp.split("\n"):
        l, r = p.split("-")
        if l in con:
            con[l].append(r)
        else:
            con[l]= [r]
        if r in con:
            con[r].append(l)
        else:
            con[r]= [l]
    return con

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
    trip = generate_triplets(pairs)
    for tpl in trip:
        for e in tpl:
            if "t" == e[0]:
                tot += 1
                break
    return tot

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
        self.assertEqual(entry_func(self.inp_str), 7)

    def test_triplets(self):
        self.assertEqual(len(generate_triplets(self.pairs)), 12)
