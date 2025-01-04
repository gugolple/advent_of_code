#!/usr/bin/env python3
import sys, itertools, unittest


def filter(rules, mine, rest):
    applied_rules = []
    for rule in rules.values():
        for ran in rule:
            applied_rules.append((ran[0], ran[1]))
    #print(applied_rules)

    good_tickets = []
    for ticket in rest:
        #print(ticket)
        for val in ticket:
            ticket_valid = False
            for s, e in applied_rules:
                if val >= s and val <= e:
                    ticket_valid = True
                    break
            if not ticket_valid:
                break
        else:
            good_tickets.append(ticket)
    return good_tickets

def contained(value: int, rules: list[list[int]]):
    #print("        ", value, rules)
    for rs, re in rules:
        if value >= rs and value <= re:
            return True
    return False

def main(rules, mine, rest, search="departure"):
    good_tickets = filter(rules, mine, rest)
    #print(good_tickets)

    found_cols = [None] * len(rules)
    pending_rules = rules.copy()

    print("Start")
#    while len(pending_rules) > 0:
    last_count = 0 
    while last_count != found_cols.count(None):
        last_count = found_cols.count(None) 
        iter_items = list(pending_rules.items())
        for pendingKey, pendingValues in iter_items:
            print(pendingKey, pendingValues)
            key_matches = 0
            key_col = -1
            # Which column matches
            for col in range(len(good_tickets[0])):
                # Skip known columns
                if found_cols[col] is not None:
                    break
                valid = True
                for ticket in good_tickets:
                    if not contained(ticket[col], pendingValues):
                        valid = False
                        break
                if valid:
                    key_col = col
                    key_matches += 1
            if key_matches == 1:
                print("Single", key_col)
                found_cols[key_col] = pendingKey
                pending_rules.pop(pendingKey, None)
            else:
                print(key_matches)
        print(found_cols)
        print()

    total = 1
    for idx,fcn in enumerate(found_cols):
        if fcn is None:
            continue
        if search in fcn:
            total *= mine[idx]

    return total


# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):

    def test_basic(self):
        rules = {
                "class": [[0,1], [4,19]],
                "row": [[0,5], [8,19]],
                "seat": [[0,13], [16,19]]
                }
        mine = [11,12,13]
        others = [
                [3,9,18],
                [15,1,5],
                [5,14,9],
                ]
        
        self.assertEqual(main(rules, mine, others, "seat"), 13)


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
