#!/usr/bin/env python3
import sys, unittest
import re

def is_valid(case, rules, rule_id=0, pos=0):
    #print("p", pos, "r", rule_id, rules[rule_id])
    for pr in rules[rule_id]:
        np = 0
        for r in pr:
            if type(r) is int:
                res, idx = is_valid(case, rules, r, pos + np)
                if not res:
                    break
                np += idx
                if (pos+np) == len(case):
                    return True, np
            else:
                if case[pos:].startswith(r):
                    #print("Accepted:", pos, np, r, case[pos+np])
                    return True, len(r)
                else:
                    #print("Failed:", pos, np, r, case[pos+np])
                    break
            #print("Accepted:", pos, np, pr)
        else:
            #print("Good:", pos, np, rule_id)
            if rule_id != 0 or np == len(case):
                return True, np

    #print("FAIL", pos, np, r, case[pos+np])
    return False, None


def main(iv):
    print()
    rules, cases = iv.split("\n\n")
    rules = dict([(int(i.split(": ")[0]), [list(map(lambda x: int(x) if x.isdigit() else x.strip("\""), j.split(" "))) for j in i.split(": ")[1].split(" | ")]) for i in rules.split("\n")])
    cases = list(cases.split("\n"))
    print(rules)

    tot = 0
    for c in cases:
        print("--------------------")
        print("Case:", c)
        if is_valid(c, rules)[0]:
            print("Good")
            tot += 1
    return tot


# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):

    def test_basic_1(self):
        iv = """0: 1 2
1: "a"
2: 1 3 | 3 1
3: "b"

aab
aba"""
        self.assertEqual(main(iv), 2)

    def test_basic_2(self):
        iv = """0: 1 2
1: "a"
2: 1 3 | 3 1
3: "b"

aaa
aab
aba
abb
baa
bab
bba
bbb"""
        self.assertEqual(main(iv), 2)

    def test_basic_3s(self):
        iv = """0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb"""
        self.assertEqual(main(iv), 1)

    def test_basic_3(self):
        iv = """0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
abbbab"""
        self.assertEqual(main(iv), 2)

    def test_basic_4(self):
        iv = """0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb"""
        self.assertEqual(main(iv), 2)


if __name__ == "__main__":
    rows = sys.stdin.read().strip()
    print(main(rows))
