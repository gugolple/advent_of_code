#!/usr/bin/env python3
import sys, itertools, unittest


def main(rules, mine, rest):
    applied_rules = []
    for rule in rules.values():
        for ran in rule:
            applied_rules.append((ran[0], ran[1]))
    print(applied_rules)

    total = 0
    for ticket in rest:
        print(ticket)
        for val in ticket:
            ticket_valid = False
            for s, e in applied_rules:
                if val >= s and val <= e:
                    ticket_valid = True
                    break
            if not ticket_valid:
                print(val)
                total += val
    return total


# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):

    def test_basic(self):
        rules = {
                "class": [[1,3], [5,7]],
                "row": [[6,11], [33,44]],
                "seat": [[13,40], [45,50]]
                }
        mine = [7,1,14]
        others = [
                [7,3,47],
                [40,4,50],
                [55,2,20],
                [38,6,12],
                ]
        self.assertEqual(main(rules, mine, others), 71)


if __name__ == "__main__":
    blocks = sys.stdin.read().strip().split("\n\n")
    rrules = blocks[0]
    rmine = blocks[1]
    rres = blocks[2]

    rules = {}
    for r in rrules.split("\n"):
        k,v = r.split(":")
        vals = [tv.strip() for tv in v.split("or")]
        ivals = [[int(i) for i in tv.split("-")] for tv in vals]
        rules[k] = ivals
        #print(k, ivals)

    mine = [int(i) for i in rmine.split("\n")[1].strip().split(",")]
    #print(mine)

    others = [[int(i) for i in to.strip().split(",")] for to in rres.split("\n")[1:]]
    #print(others)

    print(main(rules, mine, others))
