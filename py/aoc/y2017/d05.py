# PART 1


def load_jumps(inputstr):
    jumps = []
    for line in inputstr.splitlines():
        jumps.append(int(line))
    return jumps


def perform_jumps(jumps):
    jumps = jumps.copy()
    pos = 0
    steps = 0

    while 0 <= pos < len(jumps):
        next_pos = pos + jumps[pos]
        jumps[pos] += 1
        pos = next_pos
        steps += 1

    return steps


# PART 2


def perform_jumps_2(jumps):
    jumps = jumps.copy()
    pos = 0
    steps = 0

    while 0 <= pos < len(jumps):
        next_pos = pos + jumps[pos]
        if jumps[pos] >= 3:
            jumps[pos] -= 1
        else:
            jumps[pos] += 1
        pos = next_pos
        steps += 1

    return steps


def solve(inputstr):
    jumps = load_jumps(inputstr)
    steps = perform_jumps(jumps)
    print(f"Part 1: {steps}")
    steps_2 = perform_jumps_2(jumps)
    print(f"Part 2: {steps_2}")
