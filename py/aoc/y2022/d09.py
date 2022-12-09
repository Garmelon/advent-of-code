def sign(n):
    return 0 if n == 0 else n // abs(n)


def simulate_rope(input, segments):
    knots = [[0, 0] for _ in range(segments)]
    trail = {(0, 0)}
    for line in input.splitlines():
        direction, amount = line.split()
        hdx, hdy = {"L": (-1, 0), "R": (1, 0), "D": (0, -1), "U": (0, 1)}[direction]
        for _ in range(int(amount)):
            knots[0][0] += hdx
            knots[0][1] += hdy
            for i in range(1, len(knots)):
                dx, dy = knots[i - 1][0] - knots[i][0], knots[i - 1][1] - knots[i][1]
                if abs(dx) > 1 or abs(dy) > 1:
                    knots[i][0] += sign(dx)
                    knots[i][1] += sign(dy)
            trail.add(tuple(knots[-1]))
    return len(trail)


def solve(inputstr):
    print(f"Part 1: {simulate_rope(inputstr, 2)}")
    print(f"Part 2: {simulate_rope(inputstr, 10)}")
