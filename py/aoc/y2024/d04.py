def get_char(lines, x, y):
    if 0 <= y < len(lines):
        line = lines[y]
        if 0 <= x < len(line):
            return line[x]
    return ""


def is_pattern_at(lines, pattern, x, y):
    for dy, line in enumerate(pattern):
        for dx, char in enumerate(line):
            if char == " ":
                continue
            if get_char(lines, x + dx, y + dy) == char:
                continue
            return False
    return True


def count_pattern(lines, pattern):
    return sum(
        is_pattern_at(lines, pattern, x, y)
        for y, line in enumerate(lines)
        for x, _ in enumerate(line)
    )


def count_patterns(lines, patterns):
    return sum(count_pattern(lines, pattern) for pattern in patterns)


XMAS_PATTERNS = [
    [
        "XMAS",
    ],
    [
        "X",
        " M",
        "  A",
        "   S",
    ],
    [
        "X",
        "M",
        "A",
        "S",
    ],
    [
        "   X",
        "  M",
        " A",
        "S",
    ],
    [
        "SAMX",
    ],
    [
        "S",
        " A",
        "  M",
        "   X",
    ],
    [
        "S",
        "A",
        "M",
        "X",
    ],
    [
        "   S",
        "  A",
        " M",
        "X",
    ],
]

X_MAS_PATTERNS = [
    [
        "M M",
        " A ",
        "S S",
    ],
    [
        "S M",
        " A ",
        "S M",
    ],
    [
        "S S",
        " A ",
        "M M",
    ],
    [
        "M S",
        " A ",
        "M S",
    ],
]


def solve(inputstr):
    lines = inputstr.strip().split()
    print(f"Part 1: {count_patterns(lines, XMAS_PATTERNS)}")
    print(f"Part 2: {count_patterns(lines, X_MAS_PATTERNS)}")
