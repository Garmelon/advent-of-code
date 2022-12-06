def area(l, w, h):
    sides = [l * w, w * h, h * l]
    return 2 * sum(sides) + min(sides)


def ribbon(l, w, h):
    half_perimeters = [l + w, w + h, h + l]
    return 2 * min(half_perimeters) + l * w * h


def solve(inputstr):
    boxes = [tuple(map(int, line.split("x"))) for line in inputstr.splitlines()]
    part1 = sum(area(l, w, h) for l, w, h in boxes)
    print(f"Part 1: {part1}")
    part2 = sum(ribbon(l, w, h) for l, w, h in boxes)
    print(f"Part 2: {part2}")
