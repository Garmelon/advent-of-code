def score(own, elf):
    return own + 1 + elf * 3


def find_outcome(elf, own):
    return (own - elf + 1) % 3


def find_own(elf, outcome):
    return (elf + outcome - 1) % 3


def solve(inputstr):
    games = []
    for line in inputstr.splitlines():
        elf, own = line.split()
        elf = ord(elf) - ord("A")
        own = ord(own) - ord("X")
        games.append((elf, own))
    part1 = sum(score(own, find_outcome(elf, own)) for elf, own in games)
    part2 = sum(score(find_own(elf, outcome), outcome) for elf, outcome in games)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
