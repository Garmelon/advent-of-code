def solve(inputstr):
    stackstr, movestr = inputstr.split("\n\n")

    stacks = []
    for line in stackstr.splitlines()[:-1]:
        i = 0
        while line:
            crate, line = line[:3], line[4:]
            if len(stacks) <= i:
                stacks.append([])
            if crate[0] == "[":
                stacks[i].append(crate[1])
            i += 1
    for stack in stacks:
        stack.reverse()

    moves = []
    for line in movestr.splitlines():
        _, amount, _, source, _, target = line.split()
        moves.append((int(amount), int(source), int(target)))

    # Part 1
    part1 = [list(stack) for stack in stacks]
    for amount, source, target in moves:
        for _ in range(amount):
            part1[target - 1].append(part1[source - 1].pop())
    print("Part 1:", "".join(stack[-1] for stack in part1))

    # Part 2
    part2 = [list(stack) for stack in stacks]
    for amount, source, target in moves:
        part2[target - 1].extend(part2[source - 1][-amount:])
        part2[source - 1] = part2[source - 1][:-amount]
    print("Part 2:", "".join(stack[-1] for stack in part2))
