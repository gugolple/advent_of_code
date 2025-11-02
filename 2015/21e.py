#!/usr/bin/env python3
from operator import mul
import itertools
import functools
import unittest
import sys
import re
import math
from dataclasses import dataclass
from sympy import divisors
from pudb import set_trace

@dataclass
class ShopItm:
    name: str
    cost: int
    damage: int
    armor: int

    def __post_init__(self):
        self.cost = int(self.cost)
        self.damage = int(self.damage)
        self.armor = int(self.armor)

shop = '''Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3'''
weapons, armors, rings = shop.split("\n\n")
lweapons = [ShopItm(*[i for i in r.strip().split("  ") if i != ""]) for r in weapons.strip().split("\n")[1:]]
#print(lweapons)
larmors = [ShopItm(*[i for i in r.strip().split("  ") if i != ""]) for r in armors.strip().split("\n")[1:]]
larmors.append(ShopItm("Noting", 0, 0, 0))
#print(larmors)
lrings = [ShopItm(*[i for i in r.strip().split("  ") if i != ""]) for r in rings.strip().split("\n")[1:]]
lrings.append(ShopItm("Noting", 0, 0, 0))
lrings.append(ShopItm("Noting", 0, 0, 0))
#print(lrings)

@dataclass
class Stats:
    hp: int
    damage: int
    armor: int

    def hit(self, oth):
        hpr = oth.damage - self.armor
        hpr = hpr if hpr > 0 else 1
        self.hp -= hpr

def fight(player, boss):
    pohp = player.hp
    bohp = boss.hp
    cntRound = 0
    while player.hp > 0 and boss.hp > 0:
        boss.hit(player)
        if boss.hp <= 0:
            break
        player.hit(boss)
    print("FR", player, boss)
    playerWon = player.hp>0
    player.hp = pohp
    boss.hp = bohp
    return playerWon

def entry_func(inp_str):
    liststs = []
    for l in inp_str.strip().split("\n"):
        n, v = l.strip().split(": ")
        liststs.append(int(v))
    boss = Stats(*liststs)
    print("BSTS", boss)

    playerhp = 100
    bcost = None
    # Loadouts
    for w in lweapons:
        for a in larmors:
            for ir1, r1 in enumerate(lrings):
                for ir2, r2 in enumerate(lrings):
                    if ir1 == ir2:
                        continue
                    cost = w.cost + a.cost + r1.cost + r2.cost
                    damage = w.damage + a.damage + r1.damage + r2.damage
                    armor = w.armor + a.armor + r1.armor + r2.armor
                    curPlayer = Stats(playerhp, damage, armor)
                    if not fight(curPlayer, boss):
                        if bcost is None or cost > bcost:
                            bcost = cost
    return bcost

class TestChallenge(unittest.TestCase):
    def test_v(self):
        testPairs = [
            ((Stats(8,5,5), Stats(12, 7, 2)), True),
        ]
        for inp, res in testPairs:
            p, b = inp
            print("Tst:", p, b, res)
            self.assertEqual(fight(p, b), res)

    def test_basic(self):
        testPairs = [
                ('''Hit Points: 100
                 Damage: 6
                 Armor: 1''', 58) 
        ]
        for inp, res in testPairs:
            t = inp
            print("Tst:", t, res)
            self.assertEqual(entry_func(t), res)

if __name__ == '__main__':
    inp_str = sys.stdin.read()
    print(entry_func(inp_str))
