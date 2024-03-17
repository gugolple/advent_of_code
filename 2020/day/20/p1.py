#!/usr/bin/env python3
import sys, unittest
from functools import reduce
from collections import namedtuple

Square = namedtuple('Square', ['idn', 'sides'])

def string_to_int(string):
    res = 0
    for i,v in enumerate(string):
        if v == '#':
            res += 1<<i
    return res

def convert_to_squares(images):
    squares = []
    for img in images:
        left = ''.join([r[0] for r in img[1:]])
        right = ''.join([r[-1] for r in img[1:]])
        s = [img[1], img[-1], left, right]
        s.extend([r[::-1] for r in s])
        s = set([string_to_int(i) for i in s])
        sq = Square(idn=int(img[0].split(" ")[1][0:-1]), sides=s)
        squares.append(sq)
        print(img)
        print(sq)
    return squares

def main(iv):
    print()
    images = [img.split("\n") for img in iv.split("\n\n")]
    squares = convert_to_squares(images)
    hits = dict()
    for fi, fv in enumerate(squares):
        for sv in squares[fi+1:]:
            intersec = fv.sides.intersection(sv.sides)
            if len(intersec) > 0:
                print(fv.idn, sv.idn, intersec)
                if fv.idn in hits:
                    hits[fv.idn] += 1
                else:
                    hits[fv.idn] = 1

                if sv.idn in hits:
                    hits[sv.idn] += 1
                else:
                    hits[sv.idn] = 1
    print(hits)
    corners = []
    for k, v in hits.items():
        if v == 2:
            corners.append(k)
    print(corners)
    if len(corners) == 4:
        return reduce(lambda x,y: x*y, corners, 1)
    return None

# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):
    def test_basic(self):
        indat = """Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."""
        self.assertEqual(main(indat), 20899048083289)

if __name__ == "__main__":
    rows = sys.stdin.read().strip()
    print(main(rows))
