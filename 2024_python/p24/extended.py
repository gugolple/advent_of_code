import unittest, sys
from collections import deque
from copy import deepcopy

def andf(e1, e2):
    return e1 & e2

def xorf(e1, e2):
    return e1 ^ e2

def orf(e1, e2):
    return e1 | e2

def addf(e1, e2):
    return e1 + e2

gate_solv = {
        "AND": andf,
        "XOR": xorf,
        "OR": orf,
        "ADD": addf,
    }

def process_raw_inp(inp: str):
    wire_desc, gates = inp.split("\n\n")
    wire_desc = [x.split(": ") for x in wire_desc.split("\n")] 
    known_wires = dict()
    for w, s in wire_desc:
        if w in known_wires:
            print("Repeated known wire!", w, s)
            exit(1)
        known_wires[w] = int(s)
    #print(known_wires)
    unkown_gates = dict()
    for gate in gates.split("\n"):
        gat, res = gate.split(" -> ")
        wl, gt, wr = gat.split(" ")
        wl, wr = sorted([wl, wr])
        if res not in unkown_gates:
            gtdes = (wl, gt, wr)
            unkown_gates[res] = gtdes
            #unkown_gates[gtdes] = res
        else:
            print("No output wire has two sources")
            print(gate)
            exit(1)
    #print(unkown_wires)
    #print(unkown_gates)
    return known_wires, unkown_gates

def get_max_digit(known_wires):
    max_digit = 0
    for e in known_wires.keys():
        if e[0] == 'x':
            cd = int(e[1:])
            if cd > max_digit:
                max_digit = cd
    return max_digit

def check_dir_xor(unkown_gates, xy, ops):

################################################################################
########## Current layout
# X XOR1 Y -> ADDBIT // Direct findable
# CARRY_IN XOR2 ADDBIT -> Z // Recurse
# X AND1 Y -> ANDBIT1 // Direct findable
# ADD_BIT AND2 CARRY_IN -> ANDBIT2
# ANDBIT1 OR ANDBIT2 -> CARRY_OUT
################################################################################
def dirsolv(xy, pop, match_op)
    x, y = xy
    l, op, r = pop
    return op == match_op and x == l and y == r

def xor1(dig, pop)
    act_dig = f"{dig:02d}"
    return dirsolv(("x" + act_dig, "y" + act_dig), pop, "XOR")

def and1(dig, pop)
    act_dig = f"{dig:02d}"
    return dirsolv(("x" + act_dig, "y" + act_dig), pop, "AND")

def carry_out(unkown_gates, dig, pop):
    if dig == 0:
        return and1(dig, pop)
    w1, gt, w2 = pop
    if gt != "OR":
        return False
    dand = None
    cand = None
    # One of the wires is the direct and of the one lesser
    if and1(dig-1, w1):
        dand = w1
        cand = w2
    elif and1(dig-1, w2):
        dand = w2
        cand = w1
    else:
        return False
    # cand MUST be one side a DIRECT XOR
    dxor = None
    rc = None
    w1, gt, w2 = unkown_gates[cand]
    if gt != "AND":
        return False
    if xor1(dig-1, w1):
        dxor = w1
        rc = w2
    elif xor1(dig-1, w2):
        dxor = w2
        rc = w1
    else:
        return False
    return carry_out(unkown_gates, dig-1, rc)

def check_add(unkown_gates, dig):
    ea = "z" + act_dig
    cg = unkown_gates[ea]
    x, gt, y = cg
    if ea == 'z00':
        return gt == 'XOR' and sorted([x, y]) == ['x00', 'y00']
    ux = unkown_gates[x]
    uy = unkown_gates[y]
    print(dig, ea, cg, ux, uy)
    add_wire = None
    carry_wire = None
    if check_dir_xor(unkown_gates, xy, ux):
        add_wire = x
        carry_wire = y
    elif check_dir_xor(unkown_gates, xy, uy):
        add_wire = y
        carry_wire = x
    else:
        return False
    print("ADDw", add_wire)
    print("CRYw", carry_wire)
    return True

def solve_all_add(unkown_gates, max_digit):
    swps = []
    for dig in range(max_digit+1):
        if not check_add(unkown_gates, dig):
            break
    return swps


def entry_func(inp: str, func: str = "ADD"):
    known_wires, unkown_gates = process_raw_inp(inp)
    res = None
    if func == "ADD":
        res = solve_all_add(unkown_gates, get_max_digit(known_wires))
    return ",".join(res)

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example_add_all_correct(self):
        inp_str = """x00: 1
x01: 0
x02: 1
y00: 1
y01: 0
y02: 0

x00 XOR y00 -> z00
x00 AND y00 -> jfw
x01 XOR y01 -> gnj
x01 AND y01 -> ntt
jfw XOR gnj -> z01
gnj AND jfw -> spq
ntt OR spq -> ndd
x02 XOR y02 -> jgw
x02 AND y02 -> dpc
ndd XOR jgw -> z02
jgw AND ndd -> qnq
qnq OR dpc -> z03"""
        self.assertEqual(entry_func(inp_str, "ADD"), "")

    def test_example_add_all_correct(self):
        inp_str = """x00: 1
x01: 0
x02: 1
y00: 1
y01: 0
y02: 0

x00 XOR y00 -> z00
x00 AND y00 -> jfw
x01 XOR y01 -> gnj
x01 AND y01 -> ntt
jfw XOR gnj -> z01
gnj AND jfw -> spq
ntt OR spq -> ndd
x02 XOR y02 -> jgw
x02 AND y02 -> dpc
ndd XOR jgw -> z02
jgw AND ndd -> qnq
qnq OR dpc -> z03"""
        self.assertEqual(entry_func(inp_str, "ADD"), "")
