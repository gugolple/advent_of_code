import unittest, sys

def calculate_equations(a, b, d):
    tot = 0
    print(a, b, d)
    Ax, Ay = a
    Bx, By = b
    Dx, Dy = d
    # Two system equation:
    # Dx = Ax*X + Bx*Y
    # Dy = Ay*X + By*Y

    # Solved 2 system equation
    da = Ay/Ax
    Y = (da * Dx - Dy)/(da * Bx - By)
    X = (Dx - Bx * Y)/Ax
    # Force to integer
    X = round(X)
    Y = round(Y)
    print(X, Y)

    # Basic check of solution
    if Dx == (Ax*X + Bx*Y) and Dy == (Ay*X + By*Y):
        tot = 3*X + Y
    return tot

def entry_func(inp: str):
    tot = 0
    for game in inp.split("\n\n"):
        gs = [i.split(": X")[1] for i in game.split("\n")]
        a = [int(i) for i in gs[0].lstrip("+").split(", Y+")]
        b = [int(i) for i in gs[1].lstrip("+").split(", Y+")]
        d = [int(i)+10000000000000 for i in gs[2].lstrip("=").split(", Y=")]
        tot += calculate_equations(a, b, d)
    return tot

if __name__ == "__main__":
    inp_str = sys.stdin.read()
    print(entry_func(inp_str[:-1]))

class AllTestCase(unittest.TestCase):
    def test_example(self):
        inp_str = """Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"""
        self.assertEqual(entry_func(inp_str), 480)
