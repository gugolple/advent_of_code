#!/usr/bin/env python3
import sys, unittest
import re

def aux_rec(ops):
    print(ops)
    left = None
    op = ops[0]
    if type(op) is list:
        left = aux_rec(op)
    elif op.isdigit():
        left = int(op)
    else:
        raise Exception(f"Bad data!: {op}")
    for op, right in zip(ops[1::2], ops[2::2]):
        print(op, right)
        rv = None
        if type(right) is list:
            rv = aux_rec(right)
        else:
            rv = int(right)
        
        if op == "+":
            left += rv
        elif op == "*":
            left *= rv
        else:
            raise Exception(f"Bad pair!: {op} -- {right}")
    return left


def parenthesis_to_lists(ops):
    print("--", ops)
    tmp = [] 
    idx = 0
    while idx < len(ops):
        itm = ops[idx]
        if itm == "(":
            sub_sec, final_idx = parenthesis_to_lists(ops[idx+1:])
            idx += final_idx + 1
            tmp.append(sub_sec)
        elif itm == ")":
            return tmp, idx
        else:
            tmp.append(itm)
        idx += 1
    return tmp, len(ops)

def prioritize_element(ops, elem="+"):
    print("Priority")
    print("Input", ops)
    
    i = 0
    while i < len(ops):
        ce = ops[i]
        if type(ce) is list:
            ops[i] = prioritize_element(ce)
        i += 1
    print()
    i = 0
    while i < len(ops):
        ce = ops[i]
        if ce == "+":
            print(ops[i-1:i+2])
            ops[i-1:i+2] = [ops[i-1:i+2]]
            i -= 1
        i += 1
    print()

    print("Out", ops)
    return ops


def main(rows):
    tot = 0
    for r in rows:
        r1 = r.replace("(", "( ")
        r2 = r1.replace(")", " )")
        ops = r2.split(" ")
        recursed = parenthesis_to_lists(ops)[0]
        prior = prioritize_element(recursed)
        ttot = aux_rec(prior)
        tot += ttot
    return tot


# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):

    values = [
            ("1 + 2 * 3 + 4 * 5 + 6", 231),
            ("1 + (2 * 3) + (4 * (5 + 6))", 51),
            ("2 * 3 + (4 * 5)", 46),
            ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445),
            ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060),
            ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340),
            ]

    def test_basic(self):
        tot = 0
        print()
        for v, r in self.values:
            print(v)
            self.assertEqual(main([v]), r)
            tot += r
        print("Full")
        raw_vals = [i[0] for i in self.values]
        self.assertEqual(main(raw_vals), tot)


if __name__ == "__main__":
    rows = sys.stdin.read().strip().split("\n")
    print(main(rows))
