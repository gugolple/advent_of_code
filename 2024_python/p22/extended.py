import unittest, sys
from math import floor

def prune(num: int):
    return num%16777216

def mix(s: int, v: int):
    return s^v

def mix_prune(s: int, v: int):
    return prune(mix(s, v))

def calc_next_secret_step1(old_secret: int):
    t = old_secret * 64
    t = mix(t, old_secret)
    t = prune(t)
    return t

def calc_next_secret_step2(old_secret: int):
    t = floor(old_secret / 32)
    t = mix(t, old_secret)
    t = prune(t)
    return t

def calc_next_secret_step3(old_secret: int):
    t = old_secret*2048
    t = mix(t, old_secret)
    t = prune(t)
    return t

def calc_next_secret(old_secret: int):
    t = calc_next_secret_step1(old_secret)
    t = calc_next_secret_step2(t)
    t = calc_next_secret_step3(t)
    return t

def calc_nth_secret(start: int, iters: int):
    cv = start
    for itr in range(iters):
        t = calc_next_secret(cv)
        #print(itr, cv, t)
        cv = t
    return cv

def calc_bananas(scr: int):
    return scr%10

def generate_bananas(start: int):
    cv = start
    while True:
        cb = calc_bananas(cv)
        yield cb
        cv = calc_next_secret(cv)

def calc_delta(gnr):
    ini = next(gnr)
    while gnr:
        nv = next(gnr)
        yield nv - ini
        ini = nv

def rotate_list(l, nv):
    for idx in range(len(l)-1):
        l[idx] = l[idx+1]
    l[-1] = nv

def generate_seq(start, ws = 4):
    wsr = ws + 1
    gb = generate_bananas(start)
    window = [next(gb) for _ in range(wsr)]
    deltas = [window[idx+1]-window[idx] for idx in range(len(window)-1)]
    while True:
        yield deltas, window[-1]
        rotate_list(window, next(gb))
        rotate_list(deltas, window[-1]-window[-2])


def calc_transitions(all_transitions_seen: set, start: int, iters: int):
    res = dict()
    gw = generate_seq(start)
    #print(start)
    for itr in range(iters-6):
        seq, rv = next(gw)
        #print(seq, rv)
        seq = tuple(seq)
        if seq not in res:
            res[seq] = rv
        if seq not in all_transitions_seen:
            all_transitions_seen.add(seq)
    return res

def entry_func(inp: str, iters=2000):
    ats = set()
    monkeys = inp.split("\n")
    monkey_res = [None] * len(monkeys)
    for idx, l in enumerate(monkeys):
        monkey_res[idx] = calc_transitions(ats, int(l), iters)
    print("Preprocessed")
    bsq = 0
    bssq = None
    for seq in ats:
        cbsq = 0
        for mk in monkey_res:
            if seq in mk:
                cbsq += mk[seq]
        if cbsq > bsq:
            bsq = cbsq
            bssq = seq
    print(bsq, bssq)
    return bsq

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """1
10
100
2024"""
        self.assertEqual(entry_func(inp_str, 1993), 23)

    def test_example_frac(self):
        inp = [
                (1, 8685429),
                (10, 4700978),
                (100, 15273692),
                (2024, 8667524),
                ]
        for i, d in inp:
            self.assertEqual(calc_nth_secret(i, 2000), d)

    def test_mix(self):
        self.assertEqual(mix(42, 15), 37)

    def test_prune(self):
        self.assertEqual(prune(100000000), 16113920)

    def test_secrets(self):
        inp = [
                123,
                15887950,
                16495136,
                527345,
                704524,
                1553684,
                12683156,
                11100544,
                12249484,
                7753432,
                5908254
            ]
        for idx in range(len(inp)-1):
            cv = inp[idx]
            er = inp[idx+1]
            self.assertEqual(calc_next_secret(cv), er)

    def test_banana_prices(self):
        sv = 123
        inp = [3, 0, 6, 5, 4, 4, 6, 4, 4, 2]
        for eb in inp:
            self.assertEqual(calc_bananas(sv), eb)
            sv = calc_next_secret(sv)

    def test_banana_prices_gen(self):
        sv = 123
        inp = [3, 0, 6, 5, 4, 4, 6, 4, 4, 2]
        for cv, eb in zip(generate_bananas(sv), inp):
            self.assertEqual(cv, eb)

    def test_banana_delta_gen(self):
        sv = 123
        inp = [-3, 6, -1, -1, 0, 2, -2, 0, -2]
        for cv, eb in zip(calc_delta(generate_bananas(sv)), inp):
            self.assertEqual(cv, eb)

    def test_banana_seq(self):
        sv = 123
        inp = [-3, 6, -1, -1, 0, 2, -2, 0, -2]
        for cv, eb in zip(generate_seq(sv), inp):
            self.assertEqual(cv[0][0], eb)
