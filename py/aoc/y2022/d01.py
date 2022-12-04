def solve(inputstr):
    elves = []
    for elfstr in inputstr.strip().split("\n\n"):
        elf = sum(int(cal) for cal in elfstr.split())
        elves.append(elf)
    elves.sort()
    print(f"Part 1: {elves[-1]}")
    print(f"Part 2: {sum(elves[-3:])}")
