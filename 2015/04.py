#!/usr/bin/env python3
import unittest
import sys
import hashlib
#from pudb import set_trace; set_trace()

def entry_func(inp_str) -> int:
    inp_str = inp_str.strip()
    ansv = 0
    while True:
        cans = hashlib.md5(bytes(str(inp_str + str(ansv)).encode("ascii"))).hexdigest()
        if cans[:5] == "00000":
            break
        ansv += 1
    return ansv


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ("abcdef", 609043),
            ("pqrstuv", 1048970),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
