# PART 1


def spiral(nth):
    length = 1
    nth -= 1
    x = 0
    y = 0
    while True:
        x += min(length, nth)
        nth -= min(length, nth)
        if nth == 0:
            return x, y
        y += min(length, nth)
        nth -= min(length, nth)
        if nth == 0:
            return x, y

        length += 1

        x -= min(length, nth)
        nth -= min(length, nth)
        if nth == 0:
            return x, y
        y -= min(length, nth)
        nth -= min(length, nth)
        if nth == 0:
            return x, y

        length += 1


def manhattan(x, y):
    return abs(x) + abs(y)


# PART 2


def store(cap):
    stored = {(0, 0): 1}
    nth = 2
    while True:
        x, y = spiral(nth)
        adjacent = 0
        adjacent += stored.get((x - 1, y - 1), 0)
        adjacent += stored.get((x, y - 1), 0)
        adjacent += stored.get((x + 1, y - 1), 0)
        adjacent += stored.get((x - 1, y), 0)
        adjacent += stored.get((x + 1, y), 0)
        adjacent += stored.get((x - 1, y + 1), 0)
        adjacent += stored.get((x, y + 1), 0)
        adjacent += stored.get((x + 1, y + 1), 0)
        stored[(x, y)] = adjacent
        nth += 1

        if adjacent > cap:
            return adjacent


def solve(inputstr):
    nth = int(inputstr.strip())
    x, y = spiral(nth)
    dist = manhattan(x, y)
    print(f"Part 1: {dist}")
    adjacent = store(nth)
    print(f"Part 2: {adjacent}")
