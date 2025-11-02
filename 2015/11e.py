#!/usr/bin/env python3
import unittest
import sys
import re
from pudb import set_trace

LETS_CHG = {
    'a': 'b',
    'b': 'c',
    'c': 'd',
    'd': 'e',
    'e': 'f',
    'f': 'g',
    'g': 'h',
    'h': 'i',
    'i': 'j',
    'j': 'k',
    'k': 'l',
    'l': 'm',
    'm': 'n',
    'n': 'o',
    'o': 'p',
    'p': 'q',
    'q': 'r',
    'r': 's',
    's': 't',
    't': 'u',
    'u': 'v',
    'v': 'w',
    'w': 'x',
    'x': 'y',
    'y': 'z',
    'z': 'a',
}

INV_CHARS = set([
    "i",
    "o",
    "l",
])

def validate(pwd) -> bool:
    # Search for a dubble
    stair = 0
    staircheck = False
    for idx, l in enumerate(pwd):
        if l in INV_CHARS:
            return False
        if idx+1 < len(pwd):
            if LETS_CHG[l] == pwd[idx+1] and pwd[idx+1] != 'a':
                stair += 1
                if stair >= 2:
                    staircheck = True
            else:
                stair = 0
    # Regex
    rgxp = re.compile("(.)\\1")
    rr = rgxp.findall(''.join(pwd))
    if len(rr) < 2 or len(set(rr)) < 2:
        return False
    #print("RR", rr)
    return staircheck

def removeInvs(l):
    invidx = None
    for idx, char in enumerate(l):
        while char in INV_CHARS:
            l[idx] = LETS_CHG[char]
            char = l[idx]
            pwd = l
            for sidx in range(idx+1, len(l)):
                l[sidx] = 'a'
            return l
    return l

def advanceChr(pwd, idx):
    nchr = LETS_CHG[pwd[idx]]
    invchr = False
    while nchr in INV_CHARS:
        nchr = LETS_CHG[nchr]
        invchr = True
    if invchr:
        for sidx in range(idx+1, len(pwd)):
            pwd[sidx] = 'a'
    pwd[idx] = nchr
    if nchr == 'a':
        pwd = advanceChr(pwd, idx-1)
    return pwd

def entry_func(inp_str, d=40):
    #set_trace()
    for l in inp_str.strip().split("\n"):
        l = list(l.strip())
    pwd = removeInvs(l[:])
    #print(l, pwd)
    while not validate(pwd):
        pwd = advanceChr(pwd, len(pwd)-1)
        #print(pwd)
    print(l)
    print(pwd)
    return ''.join(pwd)


class TestChallenge(unittest.TestCase):
    def test_val(self):
        testPairs = [
            ("hijklmmn", False),
            ("abbceffg", False),
            ("abbcegjk", False),
            ("abcdefgh", False),
            ("abcdffaa", True),
            ("ghijklmn", False),
            ("ghjaabcc", True),
        ]
        for inp, res in testPairs:
            print("Tst:", inp, res)
            self.assertEqual(validate(inp), res)

    def test_basic(self):
        testPairs = [
            ("abcdefgh", "abcdffaa"),
            ("ghijklmn", "ghjaabcc"),
        ]
        for inp, res in testPairs:
            print("Tst:", inp, res)
            self.assertEqual(entry_func(inp), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    rs = entry_func(inp_str)
    print(rs)
    nrs = "".join(advanceChr(list(rs), len(rs)-1))
    print(nrs)
    rs2  = entry_func(nrs)
    print(rs2)
