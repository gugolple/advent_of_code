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

def solve_gates(kw, ug):
    known_wires = deepcopy(kw)
    unkown_gates = deepcopy(ug)
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
    return calc_sol(known_wires, 'z')

def calc_sol(known_wires, ww: str):
    tot = 0
    for w in known_wires.keys():
        if w[0] == ww:
            tot += known_wires[w] * 2**int(w[1:])
    return tot

def generate_gate(uk):
    for k, v in uk.items():
        yield k, v

def generate_gate_pairs(uk):
    for p1 in generate_gate(uk):
        k1, v1 = p1
        gnr = generate_gate(uk)
        while next(gnr) != p1:
            pass
        for p2 in gnr:
            k2, v2 = p2
            #print(k1, k2)
            #print(v1, v2)
            yield [(k1, (v1[0][0], v2[0][1])), (k2, (v2[0][0], v1[0][1]))]

def entry_func(inp: str, func: str = "ADD", gate_pairs: int = 4):
    known_wires, unkown_gates = process_raw_inp(inp)
    x = calc_sol(known_wires, 'x')
    y = calc_sol(known_wires, 'y')
    ez = gate_solv[func](x, y)
    gz = solve_gates(known_wires, unkown_gates)
#    while ez != gz:
#        curuk = deepcopy(unkown_gates)
    cur_ans = None
    for g1, g2 in generate_gate_pairs(unkown_gates):
        kg1, vg1 = g1
        kg2, vg2 = g2
        #print(g1, g2)
        og1 = unkown_gates[kg1]
        og2 = unkown_gates[kg2]
        unkown_gates[kg1] = [vg1]
        unkown_gates[kg2] = [vg2]
        gz = solve_gates(known_wires, unkown_gates)
        print(ez, gz)
        cur_ans = ",".join(sorted([vg1[1], vg2[1]]))
        unkown_gates[kg1] = og1
        unkown_gates[kg2] = og2
    return cur_ans

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """x00: 0
x01: 1
x02: 0
x03: 1
x04: 0
x05: 1
y00: 0
y01: 0
y02: 1
y03: 1
y04: 0
y05: 1

x00 AND y00 -> z05
x01 AND y01 -> z02
x02 AND y02 -> z01
x03 AND y03 -> z03
x04 AND y04 -> z04
x05 AND y05 -> z00"""
        self.assertEqual(entry_func(inp_str, "AND", 2), "z00,z01,z02,z05")
