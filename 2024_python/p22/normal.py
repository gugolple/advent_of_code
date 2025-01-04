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


def entry_func(inp: str, iters=2000):
    tot = 0
    for l in inp.split("\n"):
        tot += calc_nth_secret(int(l), iters)
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """1
10
100
2024"""
        self.assertEqual(entry_func(inp_str), 37327623)

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
