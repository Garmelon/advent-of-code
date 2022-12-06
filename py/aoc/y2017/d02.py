def load_rows(inputstr):
    rows = []
    for line in inputstr.splitlines():
        digits = line.split("\t")
        row = list(map(int, digits))
        rows.append(row)
    return rows


# PART 1


def checksum(rows):
    total = 0
    for row in rows:
        total += max(row) - min(row)
    return total


# PART 2


def divide_evenly(row):
    for a in row:
        for b in row:
            if a != b and a % b == 0:
                return a // b


def checksum_even(rows):
    total = 0
    for row in rows:
        total += divide_evenly(row)
    return total


def solve(inputstr):
    rows = load_rows(inputstr)
    check = checksum(rows)
    print(f"Part 1: {check}")
    check_even = checksum_even(rows)
    print(f"Part 2: {check_even}")
