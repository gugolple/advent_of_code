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

def get_dig_str(dig):
    return f"{dig:02d}"

def verify_dir(gates, w, dig, op):
    ad = get_dig_str(dig)
    x = "x" + ad
    y = "y" + ad
    if w not in gates:
        return False
    l, gt, r = gates[w]
    return gt == op and x == l and y == r

def verify_dir_and(gates, w, dig):
    return verify_dir(gates, w, dig, "AND")

def verify_dir_xor(gates, w, dig):
    return verify_dir(gates, w, dig, "XOR")

def verify_indir_and(gates, w, dig):
    l, gt, r = gates[w]
    if gt != "AND":
        return False
    return verify_dir_xor(gates, l, dig) and verify_or(gates, r, dig) or verify_dir_xor(gates, r, dig) and verify_or(gates, l, dig)

def verify_or(gates, w, dig):
    dig -= 1
    l, gt, r = gates[w]
    if dig == 0:
        return verify_dir_and(gates, w, dig)
    if gt != "OR":
        return False
    #print("OR", w, dig)
    return verify_dir_and(gates, l, dig) and verify_indir_and(gates, r, dig) or verify_dir_and(gates, r, dig) and verify_indir_and(gates, l, dig)

def verify_z(gates, w, digit):
    x, gt, y = gates[w]
    #print(w, x, gt, y, digit)
    if gt != "XOR":
        return False
    if digit == 0:
        return verify_dir_xor(gates, w, digit)
    return verify_dir_xor(gates, x, digit) and verify_or(gates, y, digit) or verify_dir_xor(gates, y, digit) and verify_or(gates, x, digit)

def verify(gates, digit):
    return verify_z(gates, "z" + get_dig_str(digit), digit)

def verify_all(gates, digit):
    for i in range(digit+1):
        if not verify(gates, i):
            return i
    return digit+1

def test_all(gates, max_digit):
    tgt = max_digit+1
    swaps = []
    baseline = verify_all(gates, max_digit)
    #print(baseline, tgt)
    while baseline != tgt:
        for x in gates:
            for y in gates:
                #print(x, y)
                if x == y: continue
                gates[x], gates[y] = gates[y], gates[x]
                cur = verify_all(gates, max_digit)
                if cur > baseline:
                    baseline = cur
                    break
                gates[x], gates[y] = gates[y], gates[x]
            else:
                continue
            break
        swaps += [x, y]
    return swaps

def entry_func(inp: str, func: str = "ADD"):
    known_wires, gates = process_raw_inp(inp)
    res = None
    if func == "ADD":
        res = test_all(gates, get_max_digit(known_wires))
    else:
        exit(1)
    return ",".join(sorted(res))

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

    def test_example_add_with_1_swp(self):
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
ndd XOR jgw -> qnq
jgw AND ndd -> z02
qnq OR dpc -> z03"""
        self.assertEqual(entry_func(inp_str, "ADD"), "qnq,z02")

    def test_example_add_with_1_swp(self):
        inp_str = """x00: 1
x01: 0
x02: 1
y00: 1
y01: 0
y02: 0

x00 XOR y00 -> gnj
x00 AND y00 -> jfw
x01 XOR y01 -> z00
x01 AND y01 -> ntt
jfw XOR gnj -> z01
gnj AND jfw -> spq
ntt OR spq -> ndd
x02 XOR y02 -> jgw
x02 AND y02 -> dpc
ndd XOR jgw -> qnq
jgw AND ndd -> z02
qnq OR dpc -> z03"""
        self.assertEqual(entry_func(inp_str, "ADD"), "gnj,qnq,z00,z02")
