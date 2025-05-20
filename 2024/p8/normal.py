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

def calculate_y(x, m, c):
    return int(m*x + c)

def calculate_equation_params(i, s):
    # Basic line equation
    # y = m * x + c
    # Find the gradient (m)
    m = (i[1] - s[1]) / (i[0] - s[0])
    print(m)
    # Find the intersection with 0
    c = i[1] - (m * i[0])
    # Equation found
    return m, c

def calculate_other_positions(i, s):
    # Get the equation params
    m, c = calculate_equation_params(i, s)
    o1 = i[0]*2 - s[0]
    o2 = s[0]*2 - i[0]
    o1 = (o1, calculate_y(o1, m, c))
    o2 = (o2, calculate_y(o2, m, c))
    return o1, o2

def calculate_pos(positions: set[int, int]):
    positions_seen = set()
    l = list(positions)
    print(l)
    for idx, i in enumerate(l):
        for s in l[idx+1:]:
            print(i, s)
            # Calculate other positions
            o1, o2 = calculate_other_positions(i, s)
            print(o1, o2)
            positions_seen.add(o1)
            positions_seen.add(o2)
    return positions_seen

def entry_func(inp: str):
    tot = 0
    mat = [list(i) for i in inp.split("\n")]
    print(mat)
    let_pos = locate_all(mat)
    print(let_pos)
    for k in let_pos.keys():
        print(k)
        antinodes = calculate_pos(let_pos[k])
        print("Antinodes", antinodes)
        for i in antinodes:
            if i[0] >= 0 and i[0] < len(mat) and i[1] >= 0 and i[1] < len(mat[0]):
                if mat[i[0]][i[1]] != 'X':
                    mat[i[0]][i[1]] = 'X'
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
        self.assertEqual(entry_func(inp_str), 14)

    def test_minimal(self):
        inp_str = """..........
..........
..........
....A.....
..........
.....A....
..........
..........
..........
.........."""
        self.assertEqual(entry_func(inp_str), 2)
