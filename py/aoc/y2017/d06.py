# PART 1


def load_buckets(inputstr):
    return tuple(map(int, inputstr[:-1].split("\t")))


def redistribute(buckets):
    l = list(buckets)
    i = l.index(max(l))
    n = l[i]
    l[i] = 0

    while n > 0:
        i = (i + 1) % len(l)
        l[i] += 1
        n -= 1

    return tuple(l)


def find_repeat(buckets):
    cycles = 0
    states = {buckets: 0}
    while True:
        buckets = redistribute(buckets)
        cycles += 1
        if buckets in states:
            return cycles, states[buckets]
        else:
            states[buckets] = cycles


# PART 2


def loop_cycles(buckets):
    cycles, start_cycle = find_repeat(buckets)
    return cycles - start_cycle


def solve(inputstr):
    buckets = load_buckets(inputstr)
    cycles, _ = find_repeat(buckets)
    print(f"Part 1: {cycles}")
    cycles_2 = loop_cycles(buckets)
    print(f"Part 2: {cycles_2}")
