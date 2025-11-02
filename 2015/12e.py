#!/usr/bin/env python3
import unittest
import sys
import re
import json
from pudb import set_trace

KW = "red"
def rec(j):
    if type(j) is dict:
        # Ver si contengo RED
        dl = list()
        ds = False
        for k, v in j.items():
            if type(v) is dict:
                if rec(v):
                    dl.append(k)
            elif type(v) is list:
                rec(v)
            elif type(v) is str and v == KW:
                ds = True
            else:
                print("D", type(v), v)
        if ds:
            return True
        for k in dl:
            del j[k]
    elif type(j) is list:
        # Iterar y ver si hay dict
        dl = list()
        for idx, itm in enumerate(j):
            if type(itm) is dict:
                if rec(itm):
                    dl.append(idx)
            elif type(itm) is list:
                rec(itm)
            elif type(itm) is str and itm == KW:
                dl.append(idx)
            else:
                print("L", type(itm), itm)
        # Hemos encontrado dict y ha de ser borrado
        for idx in reversed(dl):
            j.pop(idx)
    # Hemos encontrado lo que queremos
    elif type(j) is str and j == KW:
        return True
    else:
        print("J", type(j), j)
    # No hemos dado con nada, olvidalo
    return False


def entry_func(inp_str, d=40):
    #set_trace()
    inp_str = inp_str.strip()
    j = json.loads(inp_str)
    #print(json.dumps(j, sort_keys=True, indent=4))
    d = rec(j)
    if d:
        return 0
    print(json.dumps(j, sort_keys=True, indent=4))
    inp_str = json.dumps(j)
    rr = re.compile("-?\\d+")
    lrs = rr.findall(inp_str)
    res = sum([int(i) for i in lrs])
    return res


class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            ('[1,2,3]', 6),
            ('[1,{"c":"red","b":2},3]', 4),
            ('{"d":"red","e":[1,2,3,4],"f":5}', 0),
            ('[1,"red",5]', 6),
            ('''[[
                    "orange",
                    {
                        "a": "yellow",
                        "b": "red",
                        "c": 156,
                        "d": 20,
                        "e": -8,
                        "f": -37
                    },
                    [
                        {
                            "a": "blue",
                            "b": 150,
                            "c": 19
                        },
                        "orange",
                        -12,
                        9
                    ]
                ]]''', 166),
        ]
        for inp, res in testPairs:
            print("Tst:", inp, res)
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
