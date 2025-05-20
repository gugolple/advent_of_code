import unittest, sys

def locate_all(mat):
    letters_pos = dict()
    for ridx, row in enumerate(mat):
        for cidx, let in enumerate(row):
            if let != '.':
                if let not in letters_pos:
                    letters_pos[let] = set()
                letters_pos[let].add((ridx, cidx))
    return letters_pos

def calculate_other_positions(i, s, limits: (int, int)):
    # Get the equation params
    deltax = s[0] - i[0]
    deltay = s[1] - i[1]
    pos = [i[0], i[1]]
    positions = list()
    # Gow down to 0 from a position
    while pos[0] >= 0 and pos[1] >= 0 and pos[0] < limits[0] and pos[1] < limits[1]:
        positions.append((pos[0], pos[1]))
        pos[0] += deltax
        pos[1] += deltay
    return positions

def calculate_pos(positions: set[int, int], limits: (int, int)):
    positions_seen = set()
    l = list(positions)
    print("Positions", positions)
    for idx, i in enumerate(l):
        for s in l:
            if i == s:
                continue
            print(i, s)
            # Calculate other positions
            seen_pos = calculate_other_positions(i, s, limits)
            for sp in seen_pos:
                positions_seen.add(sp)
    return positions_seen

def entry_func(inp: str):
    tot = 0
    mat = [list(i) for i in inp.split("\n")]
    print(mat)
    let_pos = locate_all(mat)
    limits = (len(mat), len(mat[0]))
    print(let_pos)
    for k in let_pos.keys():
        print(k)
        antinodes = calculate_pos(let_pos[k], limits)
        #print("Antinodes", antinodes)
        for i in antinodes:
            if i[0] >= 0 and i[0] < len(mat) and i[1] >= 0 and i[1] < len(mat[0]):
                if mat[i[0]][i[1]] != '+':
                    mat[i[0]][i[1]] = '+'
                    tot += 1
    for l in mat:
        print(''.join(l))
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"""
        self.assertEqual(entry_func(inp_str), 34)

    def test_minimal(self):
        inp_str = """T.........
...T......
.T........
..........
..........
..........
..........
..........
..........
.........."""
        self.assertEqual(entry_func(inp_str), 9)
