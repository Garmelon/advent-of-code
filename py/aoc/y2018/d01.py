def find_repeat(freqs):
    total = 0
    found = {total}

    while True:
        for n in freqs:
            total += n
            if total in found:
                return total
            else:
                found.add(total)


def solve(inputstr):
    freqs = [int(freq) for freq in inputstr.splitlines()]
    print(f"Part 1: {sum(freqs)}")
    print(f"Part 2: {find_repeat(freqs)}")
