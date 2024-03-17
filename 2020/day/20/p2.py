#!/usr/bin/env python3
import sys, unittest
from collections import namedtuple

Sides = namedtuple('Sides', ['nl', 'rl', 'nr', 'rr', 'nu', 'ru', 'nd', 'rd'])
Square = namedtuple('Square', ['sides', 'img', 'sid'])

def string_to_int(string):
    res = 0
    for i,v in enumerate(string):
        if v == '#':
            res += 1<<i
    return res

def convert_to_squares(images):
    squares = dict()
    for img in images:
        left = ''.join([r[0] for r in img[1:]])
        right = ''.join([r[-1] for r in img[1:]])
        s = [img[1], img[-1], left, right]
        s.extend([r[::-1] for r in s])
        s = [string_to_int(i) for i in s]
        sets = set(s)
        sides = Sides(nu=s[0], ru=s[4], nd=s[1], rd=s[5],
                      nl=s[2], rl=s[6], nr=s[3], rr=s[7])
        sq = Square(sides=sets, img=img, sid=sides)
        squares[int(img[0].split(" ")[1][0:-1])] = sq
        #print(img)
        #print(sq)
    return squares

def collision_squares(squares):
    hits = dict()
    shared_sides = dict()
    for fk, fv in squares.items():
        for sk, sv in squares.items():
            if fk <= sk:
                continue

            intersec = fv.sides.intersection(sv.sides)
            if len(intersec) > 0:
                #print(fk, sk, intersec)
                if fk in hits:
                    hits[fk] += 1
                else:
                    hits[fk] = 1

                if sk in hits:
                    hits[sk] += 1
                else:
                    hits[sk] = 1

                if fk in shared_sides:
                    shared_sides[fk].append((sk, list(intersec)))
                else:
                    shared_sides[fk] = [(sk, list(intersec))]

                if sk in shared_sides:
                    shared_sides[sk].append((fk, list(intersec)))
                else:
                    shared_sides[sk] = [(fk, list(intersec))]
    return hits, shared_sides

def find_tl(squares, corners, shared_sides):
    for c in corners:
        ms = squares[c]
        #print(ms)
        mss = shared_sides[c]
        #print("Intersecs", mss)
        possible_sides = []
        t = [possible_sides.extend(i[1]) for i in mss] 
        #print("PS", possible_sides)

        rh = None
        dh = None

        if ms.sid.nr in possible_sides:
            rh = True
        if ms.sid.rr in possible_sides:
            rh = False

        if ms.sid.nd in possible_sides:
            dh = True
        if ms.sid.rd in possible_sides:
            dh = False

        if rh is not None and dh is not None:
                #print("Top left!")
                return ms
            
        #print()
def find_corners(hits):
    corners = []
    for k, v in hits.items():
        if v == 2:
            corners.append(k)
    return corners

def construct_image(squares, hits, shared_sides):
    corners = find_corners(hits)
    print("Corners", corners)
    seed = find_tl(squares, corners, shared_sides)
    print("Seed!", seed)
    return None

def main(iv):
    print()
    images = [img.split("\n") for img in iv.split("\n\n")]
    squares = convert_to_squares(images)
    hits, shared_sides = collision_squares(squares)
    print("Hits", hits)
    print("Shared sides", shared_sides)
    img = construct_image(squares, hits, shared_sides)
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
        self.assertEqual(main(indat), 273)

if __name__ == "__main__":
    rows = sys.stdin.read().strip()
    print(main(rows))
