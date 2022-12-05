def solve(inputstr):
    stackstr, movestr = inputstr.split("\n\n")

    stacklines = stackstr.splitlines()[:-1]
    stacks = [[] for _ in range((len(stacklines[-1]) + 1) // 4)]
    for line in stacklines[::-1]:
        for i, c in enumerate(line[1::4]):
            if c != " ":
                stacks[i].append(c)

    moves = []
    for line in movestr.splitlines():
        _, amount, _, source, _, target = line.split()
        moves.append((int(amount), int(source), int(target)))

    part1 = [list(stack) for stack in stacks]
    for amount, source, target in moves:
        part1[target - 1].extend(part1[source - 1][-amount:][::-1])
        part1[source - 1] = part1[source - 1][:-amount]
    print("Part 1:", "".join(stack[-1] for stack in part1))

    part2 = [list(stack) for stack in stacks]
    for amount, source, target in moves:
        part2[target - 1].extend(part2[source - 1][-amount:])
        part2[source - 1] = part2[source - 1][:-amount]
    print("Part 2:", "".join(stack[-1] for stack in part2))
