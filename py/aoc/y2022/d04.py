def solve(inputstr):
    part1 = 0
    part2 = 0
    for line in inputstr.splitlines():
        elf1, elf2 = line.split(",")
        s1, e1 = map(int, elf1.split("-"))
        s2, e2 = map(int, elf2.split("-"))
        if s1 <= s2 <= e2 <= e1 or s2 <= s1 <= e1 <= e2:
            part1 += 1
        if s1 <= e2 and s2 <= e1:
            part2 += 1
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
