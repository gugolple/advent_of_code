#!/usr/bin/env python3
import sys, unittest
import re

def is_valid(case, rules, rule_id=0, pos=0):
    print("p", pos, "r", rule_id, rules[rule_id])
    if pos == len(case):
        return False, None
    for pr in rules[rule_id]:
        np = 0
        for r in pr:
            if type(r) is int:
                res, idx = is_valid(case, rules, r, pos + np)
                if not res:
                    break
                np += idx
            else:
                if not case[pos:].startswith(r):
                    print("Failed:", pos, np, r, case[pos+np])
                    break
            print("Accepted:", pos, np, pr)
        else:
            print("Good:", pos, np, rule_id)
            if rule_id != 0 or np == len(case):
                return True, np

    print("FAIL", pos, np, r, case[pos+np])
    return False, None


def main(iv):
    print()
    rules, cases = iv.split("\n\n")
    rules = dict([(int(i.split(": ")[0]), [list(map(lambda x: int(x) if x.isdigit() else x.strip("\""), j.split(" "))) for j in i.split(": ")[1].split(" | ")]) for i in rules.split("\n")])
    cases = list(cases.split("\n"))
    rules[8].append([42, 8])
    rules[11].append([42, 11, 31])
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
        iv = """42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

aaaabbaaaabbaaa"""
        self.assertEqual(main(iv), 0)

    def test_basic_2(self):
        iv = """42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"""
        self.assertEqual(main(iv), 12)


if __name__ == "__main__":
    rows = sys.stdin.read().strip()
    print(main(rows))
