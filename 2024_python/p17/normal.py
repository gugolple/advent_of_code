import unittest, sys

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

def entry_func(inp: str):
    tot = 0
    state, program = inp.split("\n\n")
    program = [int(i) for i in program.lstrip("Program: ").split(",")]
    print(program)
    st = initialize_state(state)
    print(st)
    idx = st[_STATE_INSTP]
    while idx < len(program):
        ins, oper = program[idx], program[idx+1]
        st[_STATE_INSTF] = False
        #print(ins, oper)
        instructions[ins](st, oper)
        # If not jumped
        if not st[_STATE_INSTF]:
            st[_STATE_INSTP] += 2
        idx = st[_STATE_INSTP]
    print(st)
    return ",".join([str(i) for i in st[_STATE_OUT_B]]), st

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1])[0])

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"""
        out, state = entry_func(inp_str)
        self.assertEqual(out, "4,6,3,5,6,3,5,2,1,0")
    
    def test_dbg(self):
        inp_str = """Register A: 0
Register B: 0
Register C: 9

Program: 2,6"""
        out, state = entry_func(inp_str)
        self.assertEqual(state[_STATE_IDX_B], 1)
    
    def test_simpl_1(self):
        inp_str = """Register A: 10
Register B: 0
Register C: 0

Program: 5,0,5,1,5,4"""
        out, state = entry_func(inp_str)
        self.assertEqual(out, "0,1,2")
    
    def test_simpl_2(self):
        inp_str = """Register A: 2024
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"""
        out, state = entry_func(inp_str)
        self.assertEqual(out, "4,2,5,6,7,7,7,7,3,1,0")
        self.assertEqual(state[_STATE_IDX_A], 0)
    
    def test_simpl_3(self):
        inp_str = """Register A: 0
Register B: 29
Register C: 0

Program: 1,7"""
        out, state = entry_func(inp_str)
        self.assertEqual(state[_STATE_IDX_B], 26)
    
    def test_simpl_4(self):
        inp_str = """Register A: 0
Register B: 2024
Register C: 43690

Program: 4,0"""
        out, state = entry_func(inp_str)
        self.assertEqual(state[_STATE_IDX_B], 44354)
