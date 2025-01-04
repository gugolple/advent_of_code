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

    ingr_pos = []
    for ingr in ingredients:
        ts = None
        for r in rows:
            if ingr in r[1]:
                if ts is None:
                    ts = set(r[0])
                else:
                    ts = ts.intersection(set(r[0]))
        print(ingr, ts)
        ingr_pos.append([ingr, ts])
        unkowns = unkowns.difference(ts)
        print(unkowns)

    print(ingr_pos)
    finals = []
    knowns = set()
    while len(ingr_pos) > 0:
        i = 0
        while i < len(ingr_pos):
            elem, poss = ingr_pos[i]
            print(elem, poss)
            if len(poss) == 1:
                knowns.update(poss)
                ingr_pos.pop(i)
                finals.append((elem, list(poss)[0]))
                i -= 1
            else:
                ingr_pos[i][1] = poss.difference(knowns)
            i += 1

    print(finals)
    fs = sorted(finals, key=lambda tup: tup[0])
    print(fs)
    res = ','.join([i[1] for i in fs])

    return res

# To run tests: "python -m unittest -v p1.py"
class TestAdvent(unittest.TestCase):
    def test_basic(self):
        indat = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"""
        self.assertEqual(main(indat), "mxmxvkd,sqjhc,fvjkl")

if __name__ == "__main__":
    rows = sys.stdin.read().strip()
    print(main(rows))
