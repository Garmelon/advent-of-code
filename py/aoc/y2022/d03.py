def priority(item):
    return ord(item.lower()) - ord("a") + 1 + item.isupper() * 26


def chunks(things, n):
    return (things[i : i + n] for i in range(0, len(things), n))


def solve(inputstr):
    rucksacks = inputstr.splitlines()

    part1 = 0
    for rucksack in rucksacks:
        half = len(rucksack) // 2
        common = set(rucksack[:half]) & set(rucksack[half:])
        part1 += priority(common.pop())
    print(f"Part 1: {part1}")

    part2 = 0
    for r1, r2, r3 in chunks(rucksacks, 3):
        common = set(r1) & set(r2) & set(r3)
        part2 += priority(common.pop())
    print(f"Part 2: {part2}")
