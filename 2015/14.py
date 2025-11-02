#!/usr/bin/env python3
import unittest
import sys
import re
from pudb import set_trace

class Racer:
    def __init__(self, who, speed, timespeed, timerest):
        self.who = who
        self.speed = speed
        self.timespeed = timespeed
        self.timerest = timerest
        self.timecount = 0
        self.pos = 0
        self.state = 0

    def tick(self):
        self.timecount += 1
        if self.state == 0:
            # Running
            self.pos += self.speed
            if self.timecount == self.timespeed:
                self.state = 1
                self.timecount = 0
        elif self.state == 1:
            if self.timecount == self.timerest:
                self.state = 0
                self.timecount = 0


def entry_func(inp_str, iters=2503):
    lrac = list()
    for l in inp_str.strip().split('\n'):
        l = l.strip()
        f, r = l.split(", ")
        rt = int(r.removeprefix("but then must rest for ").removesuffix(" seconds."))
        who, desc = f.split(" can fly ")
        speed, ft = desc.split(" km/s for ")
        speed = int(speed)
        ft = int(ft.removesuffix(" seconds"))
        lrac.append(Racer(who, speed, ft, rt))
    for _ in range(iters):
        for rac in lrac:
            rac.tick()
    return max([rac.pos for rac in lrac])

class TestChallenge(unittest.TestCase):
    def test_basic(self):
        testPairs = [
            (('''Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
             Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.''', 1), 16),
            (('''Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
             Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.''', 10), 160),
            (('''Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
             Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.''', 12), 176),
            (('''Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
             Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.''', 1000), 1120),
        ]
        for inp, res in testPairs:
            t, tgt = inp
            print("Tst:", inp, res)
            self.assertEqual(entry_func(t, tgt), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
