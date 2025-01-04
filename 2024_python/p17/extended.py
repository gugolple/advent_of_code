import unittest, sys
from copy import deepcopy

_STATE_IDX_A = 0
_STATE_IDX_B = 1
_STATE_IDX_C = 2
_STATE_INSTP = 3
_STATE_INSTF = 4
_STATE_OUT_B = 5
_STATE_RSIZE = 6

# Division
def adv(state, op):
    opr = resolv_operand(state, op)
    a = state[_STATE_IDX_A]
    state[_STATE_IDX_A] = int(a/2**opr)

# Bitwise XOR
def bxl(state, op):
    state[_STATE_IDX_B] = state[_STATE_IDX_B] ^ op

# Combo operand % 8
def bst(state, op):
    opr = resolv_operand(state, op)
    state[_STATE_IDX_B] = opr % 8

# Jump not zero
def jnz(state, op):
    a = state[_STATE_IDX_A]
    if a != 0:
        state[_STATE_INSTP] = op
        state[_STATE_INSTF] = True

def bxc(state, op):
    state[_STATE_IDX_B] = state[_STATE_IDX_B] ^ state[_STATE_IDX_C]

def out(state, op):
    opr = resolv_operand(state, op)
    state[_STATE_OUT_B].append(opr % 8)

def bdv(state, op):
    opr = resolv_operand(state, op)
    a = state[_STATE_IDX_A]
    state[_STATE_IDX_B] = int(a/2**opr)

def cdv(state, op):
    opr = resolv_operand(state, op)
    a = state[_STATE_IDX_A]
    state[_STATE_IDX_C] = int(a/2**opr)

instructions = {
        0: adv,
        1: bxl,
        2: bst,
        3: jnz,
        4: bxc,
        5: out,
        6: bdv,
        7: cdv,
        }

instructions_txt = {
        0: "adv",
        1: "bxl",
        2: "bst",
        3: "jnz",
        4: "bxc",
        5: "out",
        6: "bdv",
        7: "cdv",
        }

operands = {
        0: 0,
        1: 1,
        2: 2,
        3: 3,
        4: ('A', _STATE_IDX_A),
        5: ('B', _STATE_IDX_B),
        6: ('C', _STATE_IDX_C)
        }

def resolv_operand(state, op):
    resolv = operands[op]
    res = resolv
    if type(resolv) == tuple:
        # Get idx of state from tuple
        res = state[resolv[1]]
    #print("Resolv oper", res)
    return res

def initialize_state(state):
    st = [0] * _STATE_RSIZE
    st[_STATE_OUT_B] = list()
    st[_STATE_INSTF] = False
    regs = [i.lstrip("Register ").split(": ") for i in state.split("\n")]
    mapping = {
            'A': _STATE_IDX_A,
            'B': _STATE_IDX_B,
            'C': _STATE_IDX_C,
            }
    for k, v in regs:
        st[mapping[k]] = int(v)
    return st

def initialize_state_zero():
    st = [0] * _STATE_RSIZE
    st[_STATE_OUT_B] = list()
    st[_STATE_INSTF] = False
    return st

def compare_equals(l1, l2):
    for e1, e2 in zip(l1, l2):
        if e1 != e2:
            return False
    return True

#def search_func(st, program, v):
#    lastlenoutbuf = 0
#    st[_STATE_IDX_A] = v
#    idx = st[_STATE_INSTP]
#    while idx < len(program):
#        ins, oper = program[idx], program[idx+1]
#        st[_STATE_INSTF] = False
#        #print(ins, oper)
#        instructions[ins](st, oper)
#        # If not jumped
#        if not st[_STATE_INSTF]:
#            st[_STATE_INSTP] += 2
#        idx = st[_STATE_INSTP]
#        clob = len(st[_STATE_OUT_B])
#        if lastlenoutbuf != clob:
#            # Compare curr char
#            if program[lastlenoutbuf] != st[_STATE_OUT_B][lastlenoutbuf]:
#                return False
#            # Should only increase by 1
#            lastlenoutbuf = clob
#    if len(program) == lastlenoutbuf:
#        return True
#    return False

def search_func(program, tgt, ans):
    if tgt == []:
        return ans
    for iv in range(8):
        st = initialize_state_zero()
        a = ans << 3 | iv
        st[_STATE_IDX_A] = a
        for idx in range(0, len(program) -2, 2):
            ins, oper = program[idx], program[idx+1]
            instructions[ins](st, oper)
        lc = st[_STATE_OUT_B]
        if lc[0] == tgt[-1]:
            r = search_func(program, tgt[:-1], a)
            if r:
                return r

def print_prog_human(program):
    for idx in range(0, len(program), 2):
        ins, op = program[idx], program[idx+1]
        print(instructions_txt[ins], operands[op])

def entry_func(inp: str):
    tot = 0
    state, program = inp.split("\n\n")
    program = [int(i) for i in program.lstrip("Program: ").split(",")]
    #print_prog_human(program)
    res = search_func(program, program, 0)
    return res

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"""
        out = entry_func(inp_str)
        self.assertEqual(out, 117440)
