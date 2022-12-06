import hashlib


def brute_force(seed, start):
    n = 1
    while True:
        text = f"{seed}{n}".encode("utf-8")
        h = hashlib.md5(text).hexdigest()
        if h.startswith(start):
            return n
        n += 1


def solve(inputstr):
    seed = inputstr.strip()
    print(f"Part 1: {brute_force(seed, '00000')}")
    print(f"Part 2: {brute_force(seed, '000000')}")
