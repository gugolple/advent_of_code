#!/usr/bin/env python3
import unittest
import sys
import hashlib
#from pudb import set_trace; set_trace()

def entry_func(inp_str, tgt=5) -> int:
    inp_str = inp_str.strip()
    ansv = 0
    tstr = ''.join("0"*tgt)
    while True:
        cans = hashlib.md5(bytes(str(inp_str + str(ansv)).encode("ascii"))).hexdigest()
        if cans[:tgt] == tstr:
            break
        ansv += 1
    return ansv


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            (("abcdef",1), 31),
            (("pqrstuv",1), 53),
        ]
        for inp, res in testPairs:
            self.assertEqual(entry_func(*inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
