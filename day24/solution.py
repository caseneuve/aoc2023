import re
import sympy


def parse(lines):
    return [map(int, re.findall(r"-?\d+", l)) for l in lines]


def solve2(input):
    x, y, z, vx, vy, vz = sympy.symbols("x, y, z, vx, vy, vz")
    equations = []
    for i, (xx, yy, zz, vvx, vvy, vvz) in enumerate(input):
        equations.append((x - xx) * (vvy - vy) - (y - yy) * (vvx - vx))
        equations.append((y - yy) * (vvz - vz) - (z - zz) * (vvy - vy))
        if i < 2:
            continue
        ans = [sol for sol in sympy.solve(equations)]
        if len(ans) == 1:
            return sum(ans[0][s] for s in [x, y, z])


if __name__ == "__main__":
    with open("input.txt") as f:
        input = parse(f.readlines())
    print(f"part2: {solve2(input)}")


def test_solve2():
    test_input = """19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3""".split("\n")
    assert 47 == solve2(parse(test_input))
