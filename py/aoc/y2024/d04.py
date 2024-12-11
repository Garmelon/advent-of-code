def mirror_h(lines):
    return [line[::-1] for line in lines]


def mirror_v(lines):
    return lines[::-1]


def transpose(lines):
    return ["".join(chars) for chars in zip(*lines)]


def shift(lines):
    return [" " * i + line + " " * (len(lines) - 1 - i) for i, line in enumerate(lines)]


def count_xmas(lines):
    return sum(line.count("XMAS") for line in lines)


def solve(inputstr):
    w2e = inputstr.strip().split()
    n2s = transpose(w2e)
    nw2se = transpose(mirror_v(shift(mirror_v(w2e))))
    ne2sw = transpose(shift(w2e))

    orientations = [w2e, n2s, nw2se, ne2sw]
    orientations = orientations + [mirror_h(o) for o in orientations]

    part1 = sum(count_xmas(o) for o in orientations)
    print(f"Part 1: {part1}")
