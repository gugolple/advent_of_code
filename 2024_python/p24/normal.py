import unittest, sys
from collections import deque

def andf(e1, e2):
    return e1 & e2

def xorf(e1, e2):
    return e1 ^ e2

def orf(e1, e2):
    return e1 | e2

gate_solv = {
        "AND": andf,
        "XOR": xorf,
        "OR": orf,
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
        gat_des = (gt, res)
        #print(gat_des)
        wires = [wl, wr, res]
        # Add all gates
        wg = tuple(sorted([wl, wr]))
        if wg not in unkown_gates:
            unkown_gates[wg] = [(gat_des)]
        else:
            unkown_gates[wg].append((gat_des))
    #print(unkown_wires)
    #print(unkown_gates)
    return known_wires, unkown_gates

def solve_gates(known_wires, unkown_gates):
    dq = deque()
    while len(unkown_gates) > 0:
        dq.clear()
        for wire_desc in unkown_gates.keys():
            wl, wr = wire_desc
            if wl in known_wires and wr in known_wires:
                for gate_desc in unkown_gates[wire_desc]:
                    g, wa = gate_desc
                    known_wires[wa] = gate_solv[g](known_wires[wl], known_wires[wr])
                dq.append(wire_desc)
        for wire_desc in dq:
            del unkown_gates[wire_desc]

def calc_sol(known_wires):
    tot = 0
    for w in known_wires.keys():
        if w[0] == 'z':
            tot += known_wires[w] * 2**int(w[1:])
    return tot

def entry_func(inp: str):
    known_wires, unkown_gates = process_raw_inp(inp)
    solve_gates(known_wires, unkown_gates)
    return calc_sol(known_wires)

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example_short(self):
        inp_str = """x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02"""
        self.assertEqual(entry_func(inp_str), 4)
    
    def test_example(self):
        inp_str = """x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj"""
        self.assertEqual(entry_func(inp_str), 2024)
