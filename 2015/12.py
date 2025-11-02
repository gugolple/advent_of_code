#!/usr/bin/env python3
import unittest
import sys
import re
from pudb import set_trace

def entry_func(inp_str, d=40):
    #set_trace()
    inp_str = inp_str.strip()
    rr = re.compile("-?\\d+")
    lrs = rr.findall(inp_str)
    res = sum([int(i) for i in lrs])
    return res


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ('[1,2,3]', 6),
            ('{"a":2,"b":4}', 6),
            ('[[[3]]]', 3),
            ('{"a":{"b":4},"c":-1}', 3),
            ('{"a":[-1,1]}', 0),
            ('[-1,{"a":1}]', 0),
            ('[]', 0),
            ('{}', 0),
        ]
        for inp, res in testPairs:
            print("Tst:", inp, res)
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
