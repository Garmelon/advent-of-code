import re

CLAIM_RE = r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"


def load_claims(inputstr):
    claims = {}
    for line in inputstr.splitlines():
        match = re.fullmatch(CLAIM_RE, line)
        elf, x, y, w, h = match.groups()
        elf, x, y, w, h = int(elf), int(x), int(y), int(w), int(h)
        claims[elf] = (x, y, w, h)
    return claims


# PART 1


def count_claims(claims):
    squares = {}
    for x, y, w, h in claims.values():
        for dw in range(w):
            for dh in range(h):
                coords = (x + dw, y + dh)
                squares[coords] = squares.get(coords, 0) + 1
    return squares


def find_doubles(squares):
    return len(list(filter(lambda x: x > 1, squares.values())))


# PART 2


def is_intact(squares, x, y, w, h):
    for dw in range(w):
        for dh in range(h):
            coords = (x + dw, y + dh)
            if squares.get(coords, 0) > 1:
                return False
    return True


def find_intact_claim(claims, squares):
    for elf, (x, y, w, h) in claims.items():
        if is_intact(squares, x, y, w, h):
            return elf


def solve(inputstr):
    claims = load_claims(inputstr)
    squares = count_claims(claims)
    doubles = find_doubles(squares)
    print(f"Part 1: {doubles}")
    intact = find_intact_claim(claims, squares)
    print(f"Part 2: {intact}")
