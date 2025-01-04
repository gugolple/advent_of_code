import unittest, sys

action_dict = {
        '^': (-1, 0),
        '<': (0, -1),
        'v': (1, 0),
        '>': (0, 1),
        '\n': None
        }

def print_mat(mat):
    [print(r) for r in mat]

def find_start(state):
    for ridx, row in enumerate(state):
        for cidx, e in enumerate(row):
            if e == '@':
                return ridx, cidx
    return None

def add_positions(pos1, pos2):
    return (pos1[0]+pos2[0], pos1[1]+pos2[1]) 

def set_pos(state, pos, v):
    state[pos[0]][pos[1]] = v 

def get_pos(state, pos):
    return state[pos[0]][pos[1]]

def walk_robot(state, actions):
    pos = find_start(state)
    set_pos(state, pos, '.')
    print(pos)
    for act in actions:
        cur_act = action_dict[act]
        # Skip if is newline
        if cur_act is None:
            continue
        next_position = add_positions(pos, cur_act)
        next_tile = get_pos(state, next_position)
        print(pos, act, next_position, next_tile)
        # Skip
        if next_tile == '#':
            continue
        # Move
        elif next_tile == '.':
            pos = next_position
        # Push
        elif next_tile == 'O':
            first_clear = next_position
            while get_pos(state, first_clear) == 'O':
                first_clear = add_positions(first_clear, cur_act)
            print("FF", first_clear, get_pos(state, first_clear))
            if get_pos(state, first_clear) == '.':
                print("Push", first_clear)
                set_pos(state, first_clear, 'O')
                set_pos(state, next_position, '.')
                pos = next_position
                print_mat(state)
        # Error!
        else:
            print("Bad tile", pos, act, next_position, next_tile)
            exit(1)

def count_score(state):
    tot = 0
    for ridx, row in enumerate(state):
        for cidx, e in enumerate(row):
            if e == 'O':
                tot += ridx*100 + cidx
    return tot

def entry_func(inp: str):
    state, actions = inp.split("\n\n")
    state = [list(i) for i in state.split("\n")]
    print(actions)
    print_mat(state)

    walk_robot(state, actions)

    return count_score(state)

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example_short(self):
        inp_str = """########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"""
        self.assertEqual(entry_func(inp_str), 2028)

    def test_example(self):
        inp_str = """##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"""
        self.assertEqual(entry_func(inp_str), 10092)
