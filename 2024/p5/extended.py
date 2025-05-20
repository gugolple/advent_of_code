import unittest, sys, heapq, re, math

def append_create(l, k, v):
        if k not in l:
            l[k] = set([v])
        else:
            l[k].add(v)

def proc_rules(rules: str):
    invalids_backwards = dict()
    backwards = dict()
    for entry in rules.split("\n"):
        l,r = entry.split("|")
        l = int(l)
        r = int(r)
        append_create(invalids_backwards, l, r)
        append_create(backwards, r, l)
    print(invalids_backwards)
    print(backwards)
    return invalids_backwards, backwards

def check_oper(oper: str, invalids_backwards):
    tot = 0
    lr = [int(x) for x in oper.split(",")]
    mid = math.floor(len(lr)/2)
    fix = False
    itr_fix = True
    while itr_fix:
        itr_fix = False
        for i in range(1, len(lr)):
            if lr[i] in invalids_backwards:
                inter_sec = invalids_backwards[lr[i]] & set(lr[:i])
                if inter_sec:
                    int_val = inter_sec.pop()
                    switch_idx = lr.index(int_val)
                    print(int_val, switch_idx, i)
                    t = lr[i]
                    lr[i] = lr[switch_idx]
                    lr[switch_idx] = t
                    fix = True
                    itr_fix = True
    if fix:
        print(lr, len(lr), mid, lr[mid])
        tot = lr[mid]
    return tot

def entry_func(inp: str):
    tot = 0
    rules, operations = inp.split("\n\n")
    invalids_backwards, backwards = proc_rules(rules)
    for oper in operations.split("\n"):
        tot += check_oper(oper, invalids_backwards)
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""
        self.assertEqual(entry_func(inp_str), 123)
