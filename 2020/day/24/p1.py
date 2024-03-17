#!/usr/bin/env python3
import sys, unittest
from math import sin, cos
from collections import namedtuple

Coordinates = namedtuple('Coordinates', ['w', 'sw', 'se', 'e', 'ne', 'nw'])

def parse_value(op):
    ops_loc = {
            'w': 0,
            'sw': 1,
            'se': 2,
            'e': 3,
            'ne': 4,
            'nw': 5,
            }
    ops_val = {
            'w': 1,
            'sw': 1,
            'se': 1,
            'e': 1,
            'ne': 1,
            'nw': 1,
            }
    idx = 0
    loc = [0] * 6
    while idx < len(op):
        cl = op[idx]
        acl = None
        if cl == 's':
            acl = op[idx:idx+2]
            idx += 1
        elif cl == 'n':
            acl = op[idx:idx+2]
            idx += 1
        else:
            acl = cl
        loc[ops_loc[acl]] += ops_val[acl]
        idx += 1
    coord = Coordinates(loc[0], loc[1], loc[2], loc[3], loc[4], loc[5])
    return coord

def simplify_val(loc):
    print(loc)
    coord_x = loc.w - loc.e + cos(60) * ((loc.nw + loc.sw) - (loc.ne + loc.se))
    coord_y = sin(60) * ((loc.nw + loc.ne) - (loc.sw + loc.se))
    return coord_x, coord_y

def main(iv):
    print()
    oprs = iv.split("\n")
    blacks = set()
    for op in oprs:
        loc = parse_value(op)
        loc = tuple(simplify_val(loc))
        print(loc)
        if loc in blacks:
            blacks.remove(loc)
        else:
            blacks.add(loc)
        print()
    print()
    for b in blacks:
        print(b)
    return len(blacks) 

# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):
    def test_basic(self):
        indat = """sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"""
        self.assertEqual(main(indat), 10)

if __name__ == "__main__":
    rows = sys.stdin.read().strip()
    print(main(rows))
