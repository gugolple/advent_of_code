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

def print_mat_rob(mat, pos):
    set_pos(mat, pos, '@')
    print_mat(mat)
    set_pos(mat, pos, '.')


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

def move_possible(state, pos, act):
    # Next pos calculated
    cur_act = action_dict[act]
    results = []
    np = add_positions(pos, cur_act)
    ct = get_pos(state, pos)
    if ct == '[':
        # Recurse right
        op = add_positions(np, (0, +1))
        results.append(move_possible(state, np, act))
        results.append(move_possible(state, op, act))
    elif ct == ']':
        # Recurse left
        op = add_positions(np, (0, -1))
        results.append(move_possible(state, np, act))
        results.append(move_possible(state, op, act))
    elif ct == '.':
        results = [True]
    elif ct == '#':
        results = [False]
    else:
        # Invalid block
        print("Invalid bloc move possible", state, pos, act, ct)
        exit(1)
    print("MovPos", pos, ct, results)
    return all(results)

def move_blocks(state, pos, act):
    # Next pos calculated
    cur_act = action_dict[act]
    results = []
    np = add_positions(pos, cur_act)
    ct = get_pos(state, pos)
    if ct == '[':
        # Recurse right
        mov = (0, +1)
        op = add_positions(np, mov)
        move_blocks(state, np, act)
        set_pos(state, np, '[')
        move_blocks(state, op, act)
        set_pos(state, op, ']')
        set_pos(state, pos, '.')
        set_pos(state, add_positions(pos, mov), '.')
    elif ct == ']':
        # Recurse left
        mov = (0, -1)
        op = add_positions(np, mov)
        move_blocks(state, np, act)
        set_pos(state, np, ']')
        move_blocks(state, op, act)
        set_pos(state, op, '[')
        set_pos(state, pos, '.')
        set_pos(state, add_positions(pos, mov), '.')
    elif ct == '.':
        print("Terminate recursion")


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
        elif next_tile == '[' or next_tile == ']':
            if act == '<' or act == '>':
                first_clear = next_position
                curfirclrtil = get_pos(state, first_clear)
                while curfirclrtil == '[' or curfirclrtil == ']':
                    first_clear = add_positions(first_clear, cur_act)
                    curfirclrtil = get_pos(state, first_clear)
                print("FF", first_clear, get_pos(state, first_clear))
                if curfirclrtil == '.':
                    print("Push", first_clear)
                    del state[first_clear[0]][first_clear[1]]
                    state[first_clear[0]].insert(next_position[1], '.')
                    pos = next_position
                    print_mat(state)
            else:
                if move_possible(state, next_position, act):
                    move_blocks(state, next_position, act)
                    print_mat(state)
                    pos = next_position
                    print_mat_rob(state, pos)
        # Error!
        else:
            print("Bad tile", pos, act, next_position, next_tile)
            exit(1)

def count_score(state):
    tot = 0
    for ridx, row in enumerate(state):
        for cidx, e in enumerate(row):
            if e == '[':
                tot += ridx*100 + cidx
    return tot


translate_dict = {
        '#': "##",
        '@': "@.",
        '.': "..",
        'O': "[]",
        '\n': "\n",
        }

def entry_func(inp: str):
    state, actions = inp.split("\n\n")
    # Apply new rules
    state = ''.join([translate_dict[i] for i in state])
    state = [list(i) for i in state.split("\n")]
    print(actions)
    print_mat(state)

    walk_robot(state, actions)

    return count_score(state)

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
#    def test_example_short(self):
#        inp_str = """#######
##...#.#
##.....#
##..OO@#
##..O..#
##.....#
########
#
#<vv<<^^<<^^"""
#        self.assertEqual(entry_func(inp_str), 618)

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
        self.assertEqual(entry_func(inp_str), 9021)
