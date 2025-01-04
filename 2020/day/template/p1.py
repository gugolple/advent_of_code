#!/usr/bin/env python3
import sys, unittest

def main(iv):
    print(iv)
    return None

# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):
    def test_basic(self):
        self.assertEqual(main(None), 0)

if __name__ == "__main__":
    rows = sys.stdin.read().strip()
    print(main(rows))
