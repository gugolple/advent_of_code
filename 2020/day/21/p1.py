#!/usr/bin/env python3
import sys, unittest

def main(iv):
    print()
    rows = [i[:-1].split(" (contains ") for i in iv.split("\n")]
    rows = list(map(lambda x: (x[0].split(" "), x[1].split(", ")), rows))

    unkowns_dc = dict()
    unkowns = set()
    ingredients = set()

    for r in rows:
        print(r)
        unkowns.update(r[0])
        for uk in r[0]:
            if uk in unkowns_dc:
                unkowns_dc[uk] += 1
            else:
                unkowns_dc[uk] = 1
        ingredients.update(r[1])

    print(unkowns)
    print(ingredients)

    for ingr in ingredients:
        print(ingr)
        ts = None
        for r in rows:
            if ingr in r[1]:
                if ts is None:
                    ts = set(r[0])
                else:
                    ts = ts.intersection(set(r[0]))
        print(ts, unkowns)
        unkowns = unkowns.difference(ts)

    print(unkowns)
    total = 0
    for uk in unkowns:
        total += unkowns_dc[uk]

    return total

# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):
    def test_basic(self):
        indat = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"""
        self.assertEqual(main(indat), 5)

if __name__ == "__main__":
    rows = sys.stdin.read().strip()
    print(main(rows))
