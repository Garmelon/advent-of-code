def scan(s, lookback):
    for i in range(len(s) - lookback + 1):
        if len(set(s[i : i + lookback])) == lookback:
            return i + lookback


def solve(inputstr):
    print(f"Part 1: {scan(inputstr, 4)}")
    print(f"Part 2: {scan(inputstr, 14)}")
