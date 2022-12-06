def santa(directions):
    x, y = 0, 0
    houses = {(0, 0)}
    for d in directions:
        dx, dy = {"^": (0, 1), "v": (0, -1), "<": (-1, 0), ">": (1, 0)}[d]
        x, y = x + dx, y + dy
        houses.add((x, y))
    return houses


def solve(inputstr):
    directions = inputstr.strip()
    part1 = len(santa(directions))
    print(f"Part 1: {part1}")
    part2 = len(santa(directions[::2]) | santa(directions[1::2]))
    print(f"Part 2: {part2}")
